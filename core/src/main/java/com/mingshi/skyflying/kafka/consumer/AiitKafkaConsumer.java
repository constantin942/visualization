package com.mingshi.skyflying.kafka.consumer;

import com.alibaba.fastjson.JSONObject;
import com.google.protobuf.InvalidProtocolBufferException;
import com.mingshi.skyflying.analysis.IDManager;
import com.mingshi.skyflying.component.ComponentsDefine;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.SegmentRelationDao;
import com.mingshi.skyflying.dao.UserTokenDao;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.type.KeyValue;
import com.mingshi.skyflying.type.LogEntity;
import com.mingshi.skyflying.type.NodeType;
import com.mingshi.skyflying.type.RefType;
import com.mingshi.skyflying.utils.CollectionUtils;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.apache.skywalking.apm.network.language.agent.v3.SpanObject;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.kafka.support.KafkaHeaders;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.*;

@Component
@Slf4j
public class AiitKafkaConsumer {

  @Resource
  private SegmentDao segmentDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;
  @Resource
  private UserTokenDao userTokenDao;

  @KafkaListener(topics = "skywalking-segments", groupId = "skyflying-consumer-group")
  public void onMessage(ConsumerRecord<String, Bytes> record, Acknowledgment ack, @Header(KafkaHeaders.RECEIVED_TOPIC) String topic) {
    Optional message = Optional.ofNullable(record.value());
    if (message.isPresent()) {
      useNoReactorModel(record);
      ack.acknowledge();
    }
  }

  private void useNoReactorModel(ConsumerRecord<String, Bytes> record) {
    try {
      SegmentObject segmentObject = SegmentObject.parseFrom(record.value().get());

      List<Span> spanList = buildSpanList(segmentObject);
      // 组装segment；2022-04-20 16:33:48
      SegmentDo segment = setUserNameAndToken(spanList);
      // 重组span数据，返回前端使用；2022-04-20 16:49:02
      reorganizingSpans(segment, spanList);

      // 设置segment_id、trace_id；2022-04-24 14:26:12
      String parentSegmentId = getRef(segmentObject, segment);

      // 将全局 trace_id 和 segment_ids保存到表里，其目的是，将用户与这条访问链路上的各个segment绑定到一起；2022-04-24 17:32:06
      saveGlobalTraceIdAndSegmentIds(segmentObject, parentSegmentId);
      // 将组装好的segment插入到表中；2022-04-20 16:34:01
      insertSegment(segment);
    } catch (InvalidProtocolBufferException e) {
      e.printStackTrace();
    }
  }

  /**
   * <B>方法名称：saveGlobalTraceIdAndSegmentIds</B>
   * <B>概要说明：将全局 trace_id 和 segment_ids保存到表里，其目的是，将用户与这条访问链路上的各个segment绑定到一起；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年04月24日 17:04:04
   * @Param [segmentObject]
   **/
  private void saveGlobalTraceIdAndSegmentIds(SegmentObject segmentObject, String parentSegmentId) {
    SegmentRelationDo segmentRelationDo = null;
    try {
      // 之所以使用LinkedHashSet，是为了防止存在重复的数据；2022-04-24 17:44:29
      LinkedHashSet<String> linkedHashSet = new LinkedHashSet<>();
      String traceSegmentId = segmentObject.getTraceSegmentId();
      String globalTraceId = segmentObject.getTraceId();

      // 在这里之所以加独占锁，是因为测试的时候，发现Kafka的消费者竟然由单线程编程多线程的了？为了保证插入和更新操作的线程安全问题，这里加独占锁。
      // 注意：这里加独占锁只适合于单实例部署的情况。如果是多实例部署的话，需要将独占锁改成分布式独占锁，比如使用Redission的lock锁。
      synchronized (AiitKafkaConsumer.class) {
        try {
          log.info("当前线程 {}", Thread.currentThread().getName());
          segmentRelationDo = segmentRelationDao.selectByGlobalTraceId(globalTraceId);
        } catch (Exception e) {
          log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，根据全局traceId =【{}】在表中查询数据时，出现了异常。", globalTraceId);
          return;
        }
        String service = segmentObject.getService();
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("parentSegmentId", parentSegmentId == null ? "##" : parentSegmentId);
        jsonObject.put("currentSegmentId", traceSegmentId);
        jsonObject.put("service", service);
        if (null == segmentRelationDo) {
          segmentRelationDo = new SegmentRelationDo();
          segmentRelationDo.setGlobalTraceId(globalTraceId);
          linkedHashSet.add(jsonObject.toJSONString());
          segmentRelationDo.setSegmentIds(JsonUtil.obj2String(linkedHashSet));
          int insertReslut = segmentRelationDao.insertSelective(segmentRelationDo);
          if (1 != insertReslut) {
            log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，将全局traceId和对应的segmentIds插入到表中失败。【{}】。", JsonUtil.obj2String(segmentRelationDo));
          }
        } else {
          String segmentIds = segmentRelationDo.getSegmentIds();
          if (StringUtil.isEmpty(segmentIds)) {
            log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，根据全局traceId在表中找到了对应的记录，但该记录没有设置对应的segmentId。这是不正常的。【{}】。", JsonUtil.obj2String(segmentRelationDo));
            return;
          }
          // TODO: 在这里将segmentId放到parentSegmentId的后面；2022-04-24 17:58:07
          linkedHashSet = JsonUtil.string2Obj(segmentIds, LinkedHashSet.class);
          linkedHashSet.add(jsonObject.toJSONString());
          segmentRelationDo.setSegmentIds(JsonUtil.obj2String(linkedHashSet));
          int updateResult = segmentRelationDao.updateByPrimaryKeySelective(segmentRelationDo);
          if (1 != updateResult) {
            log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，将全局traceId和对应的segmentIds更新到表中失败。【{}】。", JsonUtil.obj2String(segmentRelationDo));
          }
        }
      }
    } catch (Exception e) {
      log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，将全局traceId和对应的segmentIds更新或保存到表中失败。【{}】。", segmentRelationDo == null ? null : JsonUtil.obj2String(segmentRelationDo));
    }
  }

  private String getRef(SegmentObject segmentObject, SegmentDo segment) {
    segment.setCurrentSegmentId(segmentObject.getTraceSegmentId());
    segment.setGlobalTraceId(segmentObject.getTraceId());
    String ref = segmentObject.getRef();
    if (!StringUtil.isEmpty(ref)) {
      TraceSegmentRef traceSegmentRef = JsonUtil.string2Obj(ref, TraceSegmentRef.class);
      String parentSegmentId = traceSegmentRef.getTraceSegmentId();
      segment.setParentSegmentId(parentSegmentId);
      return parentSegmentId;
    }
    return null;
  }

  private void reorganizingSpans(SegmentDo segment, List<Span> spanList) {
    if (StringUtil.isEmpty(segment.getUserName()) && StringUtil.isEmpty(segment.getToken())) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，该调用链 = 【{}】 不含有用户名或者token，不能插入到表中。", JsonUtil.obj2String(segment));
      return;
    }

    List<String> linkedList = new LinkedList<>();
    if (CollectionUtils.isNotEmpty(spanList)) {
      List<Span> rootSpans = findRoot(spanList);
      for (Span span : rootSpans) {
        List<Span> childrenSpan = new ArrayList<>();
        childrenSpan.add(span);

        // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
        getData(span, linkedList);
        findChildrenDetail(spanList, span, childrenSpan, linkedList);
      }
      // if (CollectionUtils.isNotEmpty(rootSpans)) {
      //   rootSpans.forEach(span -> {
      //     List<Span> childrenSpan = new ArrayList<>();
      //     childrenSpan.add(span);
      //
      //     // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
      //     getData(span, linkedList);
      //     findChildrenDetail(spanList, span, childrenSpan, linkedList);
      //   });
      // }
    }

    String toString = linkedList.toString();
    segment.setReorganizingSpans(toString);
  }

  private List<Span> findRoot(List<Span> spans) {
    List<Span> rootSpans = new ArrayList<>();
    spans.forEach(span -> {
      String segmentParentSpanId = span.getSegmentParentSpanId();

      boolean hasParent = false;
      for (Span subSpan : spans) {
        if (segmentParentSpanId.equals(subSpan.getSegmentSpanId())) {
          hasParent = true;
          // if find parent, quick exit
          break;
        }
      }

      if (!hasParent) {
        span.setRoot(true);
        rootSpans.add(span);
      }
    });
    /*
     * In some cases, there are segment fragments, which could not be linked by Ref,
     * because of two kinds of reasons.
     * 1. Multiple leaf segments have no particular order in the storage.
     * 2. Lost in sampling, agent fail safe, segment lost, even bug.
     * Sorting the segments makes the trace view more readable.
     */
    rootSpans.sort(Comparator.comparing(Span::getStartTime));
    return rootSpans;
  }

  private void findChildrenDetail(List<Span> spans, Span parentSpan, List<Span> childrenSpan, List<String> linkedList) {
    spans.forEach(span -> {
      if (span.getSegmentParentSpanId().equals(parentSpan.getSegmentSpanId())) {
        childrenSpan.add(span);
        getData(span, linkedList);
        findChildrenDetail(spans, span, childrenSpan, linkedList);
      }
    });
  }

  // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
  private void getData(Span span, List<String> linkedList) {
    try {
      String peer = span.getPeer();
      JSONObject jsonObject = new JSONObject();
      String component = span.getComponent();
      if (StringUtil.isEmpty(component)) {
        return;
      }
      if (component.equals("SpringMVC")) {
        String userName = span.getUserName();
        String token = span.getToken();
        if (null != userName && "" != userName) {
          jsonObject.put("userName", userName + "," + token);
        }
        if (null != token && "" != token) {
          jsonObject.put("userName", token);
        }

        List<KeyValue> tagsList = span.getTags();
        for (KeyValue keyValue : tagsList) {
          if (keyValue.getKey().equals("url")) {
            String url = keyValue.getValue();
            jsonObject.put("url", url);
            linkedList.add(jsonObject.toJSONString());
          }
        }
      } else
      if (component.equals(ComponentsDefine.MYSQL_JDBC_DRIVER.getName())) {
        jsonObject.put("ip", peer);
        List<KeyValue> tagsList = span.getTags();
        for (KeyValue keyValue : tagsList) {
          String key = keyValue.getKey();
          if (key.equals("db.instance")) {
            String dataBaseName = keyValue.getValue();
            jsonObject.put("databaseName", dataBaseName);
            jsonObject.put("dbType", "mysql");
          } else if (key.equals("db.statement")) {
            String sql = keyValue.getValue();
            jsonObject.put("sqlDetail", sql);
            String table = null;
            String sqlType = null;
            if (sql.contains("select")) {
              sqlType = "select";
              try {
                table = sql.split("from")[1].split("where")[0].split("order")[0].replace(" ", "").replace("\n", "");
              } catch (Exception e) {
                e.printStackTrace();
              }
            } else if (sql.contains("insert")) {
              sqlType = "insert";
              try {
                table = sql.split("insert")[1].split("into")[1].split("\\(")[0].replace(" ", "").replace("\n", "");
              } catch (Exception e) {
                e.printStackTrace();
              }
            } else if (sql.contains("update")) {
              sqlType = "update";
              try {
                table = sql.split("SET")[0].replace(" ", "").replace("\n", "");
              } catch (Exception e) {
                e.printStackTrace();
              }
            } else if (sql.contains("delete")) {
              sqlType = "delete";
            }
            jsonObject.put("actionType", sqlType);
            jsonObject.put("tableName", table);
          }
        }
        if (0 < jsonObject.size() && !linkedList.contains(jsonObject.toJSONString())) {
          linkedList.add(jsonObject.toJSONString());
        }
      } else if (component.equals("Lettuce/SETEX")) {
        List<KeyValue> tagsList = span.getTags();
        String dbType = null;
        String ip = span.getPeer();
        jsonObject.put("ip", ip);
        for (KeyValue keyValue : tagsList) {
          if (keyValue.getKey().equals("db.type")) {
            dbType = keyValue.getValue();
            jsonObject.put("dbType", dbType);
          } else if (keyValue.getKey().equals("db.statement")) {
            String value = keyValue.getValue();
            try {
              jsonObject.put("order", value);
              if (0 < jsonObject.size()) {
                linkedList.add(jsonObject.toJSONString());
              }
            } catch (Exception e) {
              e.printStackTrace();
            }
          }
        }
      } else if (component.equals("HttpClient")) {
        List<KeyValue> tagsList = span.getTags();
        String dbType = null;
        String ip = span.getPeer();
        jsonObject.put("ip", ip);
        if (ip.contains("dingtalk")) {
          jsonObject.put("dbType", "钉钉");
          linkedList.add(jsonObject.toJSONString());
        } else {
          for (KeyValue keyValue : tagsList) {
            if (keyValue.getKey().equals("db.type")) {
              dbType = keyValue.getValue();
              jsonObject.put("dbType", dbType);
            } else if (keyValue.getKey().equals("db.statement")) {
              String value = keyValue.getValue();
              try {
                jsonObject.put("order", value);
                if (0 < jsonObject.size()) {
                  linkedList.add(jsonObject.toJSONString());
                }
              } catch (Exception e) {
                e.printStackTrace();
              }
            }
          }
        }
      } else {
        log.info("{}", component);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  private void insertSegment(SegmentDo segment) {
    // 用户名和token都是空的调用链，不存入数据库中。这里只存入带有用户名或者token完整的调用链。2022-04-20 16:35:52
    if (StringUtil.isEmpty(segment.getUserName()) && StringUtil.isEmpty(segment.getToken())) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，该调用链 = 【{}】 不含有用户名或者token，不能插入到表中。", JsonUtil.obj2String(segment));
      return;
    } else if (StringUtil.isEmpty(segment.getUserName()) && !StringUtil.isEmpty(segment.getToken())) {
      // 如果用户名为空，但token不为空，此时要把这个token对应的用户名补全；2022-04-21 08:45:30
      boolean flag = setUserNameByToken(segment);
      if (false == flag) {
        // 如果根据token没有获取到对应的用户名，那么这条访问信息就不存入数据库中。因为没有意义，前端获取数据时，是根据用户名来获取数据的。2022-04-21 09:06:45
        // return;
      }
    } else if (!StringUtil.isEmpty(segment.getUserName()) && !StringUtil.isEmpty(segment.getToken())) {
      // 如果用户名和token都不为空，那么就把用户名和token插入到表中；2022-04-21 08:46:07
      insertUserNameAndToken(segment);
    } else if (!StringUtil.isEmpty(segment.getUserName()) && StringUtil.isEmpty(segment.getToken())) {
      // 这种情况不应该出现；2022-04-21 08:47:17
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法--将用户的调用链条信息【{}】插入到表中。但出现了异常情况，用户名不为空，但token为空。", JsonUtil.obj2String(segment));
    }

    int insertResult = segmentDao.insertSelective(segment);
    if (1 != insertResult) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，将完整的调用链 = 【{}】 插入到表中失败。", JsonUtil.obj2String(segment));
    }
  }

  /**
   * <B>方法名称：setUserNameByToken</B>
   * <B>概要说明：根据token，给segment实例设置用户名</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年04月21日 09:04:47
   * @Param [segment]
   **/
  private boolean setUserNameByToken(SegmentDo segment) {
    UserTokenDo userTokenDo = userTokenDao.selectByToken(segment.getToken());
    if (null == userTokenDo) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，根据token = 【{}】从用户名和token表中没有获取到数据，这是异常情况。", segment.getToken());
      return false;
    } else {
      segment.setUserName(userTokenDo.getUserName());
    }
    return true;
  }

  /**
   * <B>方法名称：insertUserNameAndToken</B>
   * <B>概要说明：将用户名和token信息插入到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年04月21日 08:04:30
   * @Param [segment]
   **/
  private void insertUserNameAndToken(SegmentDo segment) {
    UserTokenDo userTokenDo = new UserTokenDo();
    userTokenDo.setUserName(segment.getUserName());
    userTokenDo.setToken(segment.getToken());
    UserTokenDo userTokenDo1 = userTokenDao.selectByUserNameAndToken(userTokenDo);
    if (null == userTokenDo1) {
      int insertResult = userTokenDao.insertSelective(userTokenDo);
      if (1 != insertResult) {
        log.error("开始执行 AiitKafkaConsumer # insertUserNameAndToken()方法，将用户名和token信息【{}】插入到表中失败。", JsonUtil.obj2String(userTokenDo));
      }
    }
  }

  private List<Span> buildSpanList(SegmentObject segmentObject) {
    List<Span> spans = new ArrayList<>();

    List<SpanObject> spansList = segmentObject.getSpansList();
    for (SpanObject spanObject : spansList) {
      Span span = new Span();
      span.setUserName(spanObject.getUserName());
      span.setToken(spanObject.getToken());

      span.setTraceId(segmentObject.getTraceId());
      span.setSegmentId(segmentObject.getTraceSegmentId());
      span.setSpanId(spanObject.getSpanId());
      span.setParentSpanId(spanObject.getParentSpanId());
      span.setStartTime(spanObject.getStartTime());
      span.setEndTime(spanObject.getEndTime());
      span.setError(spanObject.getIsError());
      span.setLayer(spanObject.getSpanLayer().name());
      span.setType(spanObject.getSpanType().name());

      String segmentSpanId = segmentObject.getTraceSegmentId() + Const.SEGMENT_SPAN_SPLIT + spanObject.getSpanId();
      span.setSegmentSpanId(segmentSpanId);

      String segmentParentSpanId = segmentObject.getTraceSegmentId() + Const.SEGMENT_SPAN_SPLIT + spanObject.getParentSpanId();
      span.setSegmentParentSpanId(segmentParentSpanId);

      span.setPeer(spanObject.getPeer());

      span.setEndpointName(spanObject.getOperationName());

      span.setServiceCode(segmentObject.getService());
      span.setServiceInstanceName(segmentObject.getServiceInstance());

      String component = ComponentsDefine.getComponentMap().get(spanObject.getComponentId());
      if (!StringUtil.isEmpty(component)) {
        span.setComponent(component);
      }

      spanObject.getRefsList().forEach(reference -> {
        Ref ref = new Ref();
        ref.setTraceId(reference.getTraceId());
        ref.setParentSegmentId(reference.getParentTraceSegmentId());

        switch (reference.getRefType()) {
          case CrossThread:
            ref.setType(RefType.CROSS_THREAD);
            break;
          case CrossProcess:
            ref.setType(RefType.CROSS_PROCESS);
            break;
        }
        ref.setParentSpanId(reference.getParentSpanId());

        span.setSegmentParentSpanId(
          ref.getParentSegmentId() + Const.SEGMENT_SPAN_SPLIT + ref.getParentSpanId());

        span.getRefs().add(ref);
      });

      spanObject.getTagsList().forEach(tag -> {
        KeyValue keyValue = new KeyValue();
        keyValue.setKey(tag.getKey());
        keyValue.setValue(tag.getValue());
        span.getTags().add(keyValue);
      });

      spanObject.getLogsList().forEach(log -> {
        LogEntity logEntity = new LogEntity();
        logEntity.setTime(log.getTime());

        log.getDataList().forEach(data -> {
          KeyValue keyValue = new KeyValue();
          keyValue.setKey(data.getKey());
          keyValue.setValue(data.getValue());
          logEntity.getData().add(keyValue);
        });

        span.getLogs().add(logEntity);
      });

      spans.add(span);
    }

    return spans;
  }

  private SegmentDo setUserNameAndToken(List<Span> spanList) {
    SegmentDo segment = null;
    try {
      segment = new SegmentDo();
      segment.setSpans(JsonUtil.obj2String(spanList));
      Span span = spanList.get(spanList.size() - 1);
      segment.setOperationName(span.getEndpointName());
      segment.setRequestStartTime(DateTimeUtil.longToDate(span.getStartTime()));
      String userName = span.getUserName();
      String token = span.getToken();
      if (null != userName && "" != userName) {
        segment.setUserName(userName);
      }
      if (null != token && "" != token) {
        segment.setToken(token);
      }
      // for (int i = spanList.size() - 1; i >= 0; i--) {
      //   Boolean flag = false;
      //   Span span = spanList.get(i);
      //   segment.setRequestStartTime(DateTimeUtil.longToDate(span.getStartTime()));
      //   String userName = span.getUserName();
      //   String token = span.getToken();
      //   if (null != userName && "" != userName) {
      //     segment.setUserName(userName);
      //     flag = true;
      //   }
      //   if (null != token && "" != token) {
      //     segment.setToken(token);
      //     flag = true;
      //   }
      //   if (true == flag) {
      //     break;
      //   }
      // }
    } catch (Exception e) {
      log.error("将span对应的二进制类型的数据转换成字符串类型的数据时，出现了异常。", e);
    }
    return segment;
  }

  public SegmentDo convert(SegmentObject segmentObject) {
    SegmentDo segment = new SegmentDo();
    // NamingControl namingControl = moduleManager.find(CoreModule.NAME) ;
    String serviceId = Const.EMPTY_STRING;
    String endpointName = Const.EMPTY_STRING;
    String serviceName = Const.EMPTY_STRING;
    String endpointId = Const.EMPTY_STRING;

    // segment.setTraceId(segmentObject.getTraceId());
    // segment.setServiceInstanceId(segmentObject.getServiceInstance());
    if (StringUtil.isEmpty(serviceId)) {
      // serviceName = namingControl.formatServiceName(segmentObject.getService());
      serviceId = IDManager.ServiceID.buildId(serviceName, NodeType.Normal);
    }

    // segment.setSegmentId(segmentObject.getTraceSegmentId());
    // segment.setServiceId(serviceId);
    // segment.setServiceInstanceId(IDManager.ServiceInstanceID.buildId(serviceId, namingControl.formatInstanceName(segmentObject.getServiceInstance())));
    // segment.setLatency(duration);
    // segment.setStartTime(startTimestamp);
    // segment.setDataBinary(segmentObject.toByteArray());

    // endpointName = namingControl.formatEndpointName(serviceName, span.getOperationName());
    endpointId = IDManager.EndpointID.buildId(serviceId, endpointName);
    System.out.println("");
    return segment;
  }

  // private void insertSegmentDetail(Segment segment ) {
  //   Segment segmentRecordDetail = new Segment();
  //   segmentRecordDetail.setSegmentId(segment.getSegmentId());
  //   segmentRecordDetail.setTraceId(segment.getTraceId());
  //   segmentRecordDetail.setServiceId(segment.getServiceId());
  //   segmentRecordDetail.setServiceInstanceId(segment.getServiceInstanceId());
  //   segmentRecordDetail.setEndpointId(segment.getEndpointId());
  //   segmentRecordDetail.setStartTime(segment.getStartTime());
  //   segmentRecordDetail.setLatency(segment.getLatency());
  //   segmentRecordDetail.setIsError(segment.getIsError());
  //   segmentRecordDetail.setDataBinary(segment.getDataBinary());
  //   try {
  //     SegmentObject segmentObject = SegmentObject.parseFrom(segment.getDataBinary());
  //     List<Span> spanList = buildSpanList(segmentObject);
  //     for(int i = spanList.size() - 1; i >=0; i--){
  //       Boolean flag = false;
  //       Span span = spanList.get(i);
  //       String userName = span.getUserName();
  //       String token = span.getToken();
  //       if(null != userName && "" != userName){
  //         segment.setUserName(userName);
  //         flag = true;
  //       }
  //       if(null != token && "" != token){
  //         segment.setToken(token);
  //         flag = true;
  //       }
  //       if(true == flag){
  //         break;
  //       }
  //     }
  //   } catch (Exception e) {
  //     log.error("将span对应的二进制类型的数据转换成字符串类型的数据时，出现了异常。",e);
  //   }
  //
  //   segmentRecordDetail.setTimeBucket(source.getTimeBucket());
  //   segmentRecordDetail.setTagsRawData(source.getTags());
  //   segmentRecordDetail.setTags(Tag.Util.toStringList(source.getTags()));
  //
  //   SegmentDetailStreamProcessor.getInstance().in(segmentRecordDetail);
  // }

}
