package com.mingshi.skyflying.impl;

import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.component.ComponentsDefine;
import com.mingshi.skyflying.config.SingletonLocalStatisticsMap;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.SegmentRelationDao;
import com.mingshi.skyflying.dao.UserTokenDao;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.kafka.consumer.AiitKafkaConsumer;
import com.mingshi.skyflying.reactor.queue.BatchInsertByLinkedBlockingQueue;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.type.KeyValue;
import com.mingshi.skyflying.type.LogEntity;
import com.mingshi.skyflying.type.RefType;
import com.mingshi.skyflying.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.apache.skywalking.apm.network.language.agent.v3.SpanObject;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * <B>方法名称：SegmentConsumeServiceImpl</B>
 * <B>概要说明：清洗调用链信息</B>
 *
 * @Author zm
 * @Date 2022年05月19日 17:05:07
 * @Param
 * @return
 **/
@Slf4j
@Service("SegmentConsumerService")
public class SegmentConsumeServiceImpl implements SegmentConsumerService {
  @Resource
  private SegmentDao segmentDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;
  @Resource
  private UserTokenDao userTokenDao;

  @Override
  public ServerResponse<String> consume(ConsumerRecord<String, Bytes> record, Boolean enableReactorModelFlag) {
    SegmentObject segmentObject = null;
    try {
      segmentObject = SegmentObject.parseFrom(record.value().get());
      List<Span> spanList = buildSpanList(segmentObject);
      // 组装segment；2022-04-20 16:33:48
      SegmentDo segment = setUserNameAndToken(spanList);
      String operationName = segment.getOperationName();
      if (operationName.equals("Redisson/PING") ||
        operationName.equals("Lettuce/SENTINEL") ||
        operationName.equals("Mysql/JDBI/Connection/close") ||
        operationName.equals("GET:/devices/notification") ||
        operationName.equals("GET:/manager/html") ||
        operationName.equals("Balancer/user/checkToken") ||
        operationName.equals("GET:") ||
        operationName.equals("GET:/assets/fonts/Nunito-Bold.woff2") ||
        operationName.equals("GET:/temp/null") ||
        operationName.equals("GET:/assets/fonts/Nioicon.ttf") ||
        operationName.equals("GET:/assets/fonts/Nunito-Regular.woff2") ||
        operationName.equals("GET:/assets/fonts/Roboto-Regular.woff2") ||
        operationName.equals("GET:/assets/fonts/Roboto-Medium.woff2") ||
        operationName.equals("HikariCP/Connection/getConnection") ||
        operationName.equals("GET:/") ||
        operationName.equals("GET:/companies/companyHealth/list") ||
        operationName.equals("null:null") ||
        operationName.equals("Mysql/JDBI/PreparedStatement/executeUpdate") ||
        operationName.equals("HikariCP/Connection/close") ||

        operationName.equals("POST:/users/menusAuths") ||
        operationName.equals("GET:/zlb/getRuralCommercialBank/info") ||
        operationName.equals("Mysql/JDBI/Connection/commit") ||
        operationName.equals("Mysql/JDBI/PreparedStatement/executeUpdate") ||
        operationName.equals("HikariCP/Connection/close") ||
        operationName.equals("Mysql/JDBI/PreparedStatement/executeQuery") ||

        operationName.startsWith("SpringScheduled") ||
        operationName.equals("POST:/devices/heartbeat")) {
        return null;
      }

      // 重组span数据，返回前端使用；2022-04-20 16:49:02
      reorganizingSpans(segment, spanList);

      // 设置segment_id、trace_id；2022-04-24 14:26:12
      getRef(segmentObject, segment);
      // String parentSegmentId = getRef(segmentObject, segment);

      setSegmentIndex(segment);

      // // 将全局 trace_id 和 segment_ids保存到表里，其目的是，将用户与这条访问链路上的各个segment绑定到一起；2022-04-24 17:32:06
      // saveGlobalTraceIdAndSegmentIds(segmentObject, parentSegmentId);
      // 将组装好的segment插入到表中；2022-04-20 16:34:01
      if (true == enableReactorModelFlag) {
        doEnableReactorModel(segment);
      } else {
        // 插入segment数据；2022-05-23 10:15:22
        insertSegmentBySingle(segment);
        // 插入segment对应的index数据；2022-05-23 10:15:38
        insertIndex();
        // insertSegment(segment, segmentObject, parentSegmentId);
      }
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
    return null;
  }

  private void doEnableReactorModel(SegmentDo segment) {
    try {
      LinkedBlockingQueue linkedBlockingQueue = BatchInsertByLinkedBlockingQueue.getLinkedBlockingQueue(1, 5, segmentDao, userTokenDao);
      linkedBlockingQueue.put(segment);
    } catch (Exception e) {
      e.printStackTrace();
      log.error("将清洗好的调用链信息放入到队列中出现了异常。", e);
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
  private void saveGlobalTraceIdAndSegmentIds(SegmentObject segmentObject, String parentSegmentId, String userName, String token) {
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
          Map<String, Object> map = new HashMap<>();
          map.put("globalTraceId", globalTraceId);
          log.info("当前线程 {}", Thread.currentThread().getName());
          segmentRelationDo = segmentRelationDao.selectByGlobalTraceId(map);
        } catch (Exception e) {
          log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，根据全局traceId =【{}】在表中查询数据时，出现了异常。", globalTraceId, e);
          return;
        }
        String service = segmentObject.getService();
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("parentSegmentId", parentSegmentId == null ? "##" : parentSegmentId);
        jsonObject.put("currentSegmentId", traceSegmentId);
        jsonObject.put("service", service);
        if (null == segmentRelationDo) {
          segmentRelationDo = new SegmentRelationDo();

          if (StringUtil.isNotBlank(userName)) {
            segmentRelationDo.setUserName(userName);
          }
          if (StringUtil.isNotBlank(token)) {
            segmentRelationDo.setToken(token);
          }
          segmentRelationDo.setGlobalTraceId(globalTraceId);
          linkedHashSet.add(jsonObject.toJSONString());
          segmentRelationDo.setSegmentIds(JsonUtil.obj2String(linkedHashSet));
          int insertReslut = segmentRelationDao.insertSelective(segmentRelationDo);
          if (1 != insertReslut) {
            log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，将全局traceId和对应的segmentIds插入到表中失败。【{}】。", JsonUtil.obj2String(segmentRelationDo));
          }
        } else {
          if (StringUtil.isBlank(segmentRelationDo.getUserName())) {
            if (StringUtil.isNotBlank(userName)) {
              segmentRelationDo.setUserName(userName);
            } else if (StringUtil.isNotBlank(token)) {
              // 去用户token表中获取对应的用户名；2022-05-17 14:59:51
              UserTokenDo userTokenDo = userTokenDao.selectByToken(token);
              if (null != userTokenDo) {
                segmentRelationDo.setUserName(userTokenDo.getUserName());
              }
            }
          }
          if (StringUtil.isBlank(segmentRelationDo.getToken())) {
            if (StringUtil.isNotBlank(token)) {
              segmentRelationDo.setToken(token);
            }
          }

          String segmentIds = segmentRelationDo.getSegmentIds();
          if (StringUtil.isBlank(segmentIds)) {
            log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，根据全局traceId在表中找到了对应的记录，但该记录没有设置对应的segmentId。这是不正常的。【{}】。", JsonUtil.obj2String(segmentRelationDo));
            return;
          }
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
      log.error("开始执行 AiitKafkaConsumer # saveGlobalTraceIdAndSegmentIds()方法，将全局traceId和对应的segmentIds更新或保存到表中失败。【{}】。", segmentRelationDo == null ? null : JsonUtil.obj2String(segmentRelationDo), e);
    }
  }

  private String getRef(SegmentObject segmentObject, SegmentDo segment) {
    segment.setCurrentSegmentId(segmentObject.getTraceSegmentId());
    segment.setGlobalTraceId(segmentObject.getTraceId());
    String ref = segmentObject.getRef();
    if (!StringUtil.isBlank(ref)) {
      TraceSegmentRef traceSegmentRef = JsonUtil.string2Obj(ref, TraceSegmentRef.class);
      String parentSegmentId = traceSegmentRef.getTraceSegmentId();
      segment.setParentSegmentId(parentSegmentId);
      return parentSegmentId;
    }
    return null;
  }

  private void reorganizingSpans(SegmentDo segment, List<Span> spanList) {
    if (StringUtil.isBlank(segment.getUserName()) && StringUtil.isBlank(segment.getToken())) {
      // log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，该调用链 = 【{}】 不含有用户名或者token，不能插入到表中。", JsonUtil.obj2String(segment));
      return;
    }

    List<String> linkedList = new LinkedList<>();
    if (CollectionUtils.isNotEmpty(spanList)) {
      List<Span> rootSpans = findRoot(spanList);
      for (Span span : rootSpans) {
        List<Span> childrenSpan = new ArrayList<>();
        childrenSpan.add(span);

        // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
        // getData(span, linkedList);
        getData2(span, linkedList);
        findChildrenDetail(spanList, span, childrenSpan, linkedList);
      }
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
        // getData(span, linkedList);
        getData2(span, linkedList);
        findChildrenDetail(spans, span, childrenSpan, linkedList);
      }
    });
  }

  // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
  // private void getData(Span span, List<String> linkedList) {
  //   try {
  //     String peer = span.getPeer();
  //     JSONObject jsonObject = new JSONObject();
  //     String component = span.getComponent();
  //     String endpointName = span.getEndpointName();
  //     if (StringUtil.isBlank(component)) {
  //       return;
  //     }
  //     if (component.equals("SpringMVC") || component.equals("SpringRestTemplate")) {
  //       getSpringMVCInfo(span, jsonObject, linkedList);
  //     } else if (component.equals(ComponentsDefine.MYSQL_JDBC_DRIVER.getName())) {
  //       getDbInfo(peer, span, jsonObject, linkedList);
  //     } else if (component.equals("Lettuce/SETEX") || endpointName.equals("Lettuce/SETEX")) {
  //       getRedisInfo(span, jsonObject, linkedList);
  //     } else if (component.equals("HttpClient")) {
  //       getHttpClientInfo(span, jsonObject, linkedList);
  //     } else {
  //       log.info("{}", component);
  //     }
  //   } catch (Exception e) {
  //     e.printStackTrace();
  //   }
  // }

  /**
   * <B>方法名称：getData2</B>
   * <B>概要说明：只要span中的tags字段不为空，那么就把这个span放入到链表中。这样做的目的是：不再区分是哪个插件拦截到的信息，像skywalking的服务端一样，使用统一的展示方式在前端展示数据。</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月17日 10:05:41
   * @Param [span, linkedList]
   **/
  private void getData2(Span span, List<String> linkedList) {
    try {
      JSONObject jsonObject = new JSONObject();
      int spanId = span.getSpanId();
      if (0 == spanId) {
        getSpringMVCInfo(span, jsonObject, linkedList);
      } else if (0 < span.getTags().size()) {
        jsonObject.put("spanId", spanId);
        jsonObject.put("parentSpanId", span.getParentSpanId());
        jsonObject.put("serviceCode", span.getServiceCode());
        jsonObject.put("serviceInstanceName", span.getServiceInstanceName());
        jsonObject.put("startTime", span.getStartTime());
        jsonObject.put("endTime", span.getEndTime());
        jsonObject.put("endpointName", span.getEndpointName());
        jsonObject.put("peer", span.getPeer());
        jsonObject.put("component", span.getComponent());
        jsonObject.put("tags", span.getTags());
        // jsonObject.put("userName", span.getUserName());
        // jsonObject.put("token", span.getToken());
        linkedList.add(jsonObject.toJSONString());
        // linkedList.add(JsonUtil.obj2String(span));
      }
    } catch (Exception e) {
      log.error("将span的信息 = 【{}】放入到LinkedList中的时候，出现了异常。", JsonUtil.obj2StringPretty(span), e);
    }
  }

  /**
   * <B>方法名称：getDbInfo</B>
   * <B>概要说明：获取数据库的访问信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月12日 16:05:49
   * @Param [peer, span, jsonObject, linkedList]
   **/
  // private void getDbInfo(String peer, Span span, JSONObject jsonObject, List<String> linkedList) {
  //   jsonObject.put("ip", peer);
  //   List<KeyValue> tagsList = span.getTags();
  //   for (KeyValue keyValue : tagsList) {
  //     String key = keyValue.getKey();
  //     if (key.equals("db.instance")) {
  //       String dataBaseName = keyValue.getValue();
  //       jsonObject.put("databaseName", dataBaseName);
  //       jsonObject.put("dbType", "mysql");
  //     } else if (key.equals("db.statement")) {
  //       getSqlInfo(keyValue, jsonObject);
  //     }
  //   }
  //   if (0 < jsonObject.size() && !linkedList.contains(jsonObject.toJSONString())) {
  //     linkedList.add(jsonObject.toJSONString());
  //   }
  // }

  /**
   * <B>方法名称：getSpringMVCInfo</B>
   * <B>概要说明：获取SpringMVC信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月12日 16:05:24
   * @Param [span, jsonObject, linkedList]
   **/
  private void getSpringMVCInfo(Span span, JSONObject jsonObject, List<String> linkedList) {
    String userName = span.getUserName();
    String token = span.getToken();
    // if (null != userName && "" != userName) {
    //   jsonObject.put("userName", userName + "," + token);
    // }
    // if (null != token && "" != token) {
    //   jsonObject.put("userName", token);
    // }

    List<KeyValue> tagsList = span.getTags();
    for (KeyValue keyValue : tagsList) {
      if (keyValue.getKey().equals("url")) {
        String url = keyValue.getValue();
        jsonObject.put("url", url);
        linkedList.add(jsonObject.toJSONString());
      }
    }
  }

  /**
   * <B>方法名称：getHttpClientInfo</B>
   * <B>概要说明：获取HttpClient信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月12日 16:05:08
   * @Param [span, jsonObject, linkedList]
   **/
  // private void getHttpClientInfo(Span span, JSONObject jsonObject, List<String> linkedList) {
  //   List<KeyValue> tagsList = span.getTags();
  //   String dbType = null;
  //   String ip = span.getPeer();
  //   jsonObject.put("ip", ip);
  //   if (ip.contains("dingtalk")) {
  //     jsonObject.put("dbType", "钉钉");
  //     linkedList.add(jsonObject.toJSONString());
  //   } else {
  //     for (KeyValue keyValue : tagsList) {
  //       if (keyValue.getKey().equals("db.type")) {
  //         dbType = keyValue.getValue();
  //         jsonObject.put("dbType", dbType);
  //       } else if (keyValue.getKey().equals("db.statement")) {
  //         String value = keyValue.getValue();
  //         try {
  //           jsonObject.put("order", value);
  //           if (0 < jsonObject.size()) {
  //             linkedList.add(jsonObject.toJSONString());
  //           }
  //         } catch (Exception e) {
  //           e.printStackTrace();
  //         }
  //       }
  //     }
  //   }
  // }

  // private void getRedisInfo(Span span, JSONObject jsonObject, List<String> linkedList) {
  //   List<KeyValue> tagsList = span.getTags();
  //   String dbType = null;
  //   String ip = span.getPeer();
  //   jsonObject.put("ip", ip);
  //   for (KeyValue keyValue : tagsList) {
  //     if (keyValue.getKey().equals("db.type")) {
  //       dbType = keyValue.getValue();
  //       jsonObject.put("dbType", dbType);
  //     } else if (keyValue.getKey().equals("db.statement")) {
  //       String value = keyValue.getValue();
  //       try {
  //         jsonObject.put("actionType", value);
  //         if (0 < jsonObject.size()) {
  //           linkedList.add(jsonObject.toJSONString());
  //         }
  //       } catch (Exception e) {
  //         e.printStackTrace();
  //       }
  //     }
  //   }
  // }

  /**
   * <B>方法名称：getSqlInfo</B>
   * <B>概要说明：获取sql信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月12日 16:05:15
   * @Param [keyValue, jsonObject]
   **/
  // private void getSqlInfo(KeyValue keyValue, JSONObject jsonObject) {
  //   String sql = keyValue.getValue();
  //   List<String> tableList = new ArrayList<>();
  //   jsonObject.put("sqlDetail", sql);
  //   if (sql.contains("?")) {
  //     sql = sql.replace("\"?\"", "*");
  //   }
  //
  //   String table = null;
  //   String sqlType = null;
  //   if (sql.contains("select") || sql.contains("SELECT")) {
  //     sqlType = "select";
  //     // table = getTable(sql, sqlType);
  //     tableList = SqlParserUtils.selectTable(sql);
  //   } else if (sql.contains("insert") || sql.contains("INSERT")) {
  //     sqlType = "insert";
  //     // table = getTable(sql, sqlType);
  //     tableList = SqlParserUtils.insertTable(sql);
  //   } else if (sql.contains("update") || sql.contains("UPDATE")) {
  //     sqlType = "update";
  //     // table = getTable(sql, sqlType);
  //     tableList = SqlParserUtils.updateTable(sql);
  //   } else if (sql.contains("delete") || sql.contains("DELETE")) {
  //     sqlType = "delete";
  //     // table = getTable(sql, sqlType);
  //     tableList = SqlParserUtils.deleteTable(sql);
  //   }
  //   jsonObject.put("actionType", sqlType);
  //   jsonObject.put("tableName", JsonUtil.obj2String(tableList));
  //   // jsonObject.put("tableName", table);
  // }

  /**
   * <B>方法名称：getTable</B>
   * <B>概要说明：获取表名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年05月12日 16:05:13
   * @Param [sql]
   **/
  // private String getTable(String sql, String sqlType) {
  //   List<String> tableList = null;
  //   try {
  //     if (sql.contains("?")) {
  //       sql = sql.replace("\"?\"", "*");
  //     }
  //
  //     tableList = SqlParserUtils.selectTable(sql);
  //     if (0 < tableList.size()) {
  //       return JsonUtil.obj2String(tableList);
  //     }
  //   } catch (Exception e) {
  //     e.printStackTrace();
  //   }
  //   return null;
  // }

  /**
   * <B>方法名称：insertSegment2</B>
   * <B>概要说明：将segment数据插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月23日 10:05:54
   * @Param [segment]
   **/
  private void insertSegmentBySingle(SegmentDo segment) {
    int insertResult = segmentDao.insertSelective(segment);
    if (1 != insertResult) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，将完整的调用链 = 【{}】 插入到表中失败。", JsonUtil.obj2String(segment));
    }
  }

  /**
   * <B>方法名称：insertSegmentIndex </B>
   * <B>概要说明：将userName或者token，与globalTraceId关联起来 </B>
   * 注：userName与token是等价的。当用户第一次登录时，如果用户校验成功，那么下次用户再访问其他接口时，会使用token来代替用户名。
   *
   * @return void
   * @Author zm
   * @Date 2022年05月23日 10:05:22
   * @Param [segment]
   **/
  private void setSegmentIndex(SegmentDo segment) {
    String operationName = segment.getOperationName();
    if (operationName.equals("Redisson/PING") || operationName.equals("Lettuce/SENTINEL") || operationName.equals("Mysql/JDBI/Connection/close")) {
      return;
    }
    Map<String/* globalTraceId */, String/* userName */> globalTraceIdAndUserNameMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndUserNameMap();
    Map<String/* globalTraceId */, String/* token */> globalTraceIdAndTokenMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndTokenMapMap();
    Map<String/* token */, String/* userName */> tokenAndUserNameMap = SingletonLocalStatisticsMap.getTokenAndUserNameMap();

    String globalTraceId = segment.getGlobalTraceId();
    // 用户名和token都是空的调用链，不存入数据库中。这里只存入带有用户名或者token完整的调用链。2022-04-20 16:35:52
    String segmentUserName = segment.getUserName();
    String segmentToken = segment.getToken();
    String userName = null;
    if (StringUtil.isBlank(segmentUserName) && StringUtil.isBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
      // 当用户名和token都为null，但全局追踪id不为空；
      // 首先根据globalTraceId获取userName；
      userName = globalTraceIdAndUserNameMap.get(globalTraceId);
      if (StringUtil.isNotBlank(userName)) {
        segment.setUserName(userName);
      } else {
        SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
      }

      // 首先根据globalTraceId获取token；
      String token = globalTraceIdAndTokenMap.get(globalTraceId);
      if (StringUtil.isNotBlank(token)) {
        // 首先根据 token 获取 userName；
        userName = tokenAndUserNameMap.get(token);
        if (StringUtil.isNotBlank(userName)) {
          segment.setUserName(userName);
          SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
          globalTraceIdAndUserNameMap.put(globalTraceId, userName);
        }
      }
    } else if (StringUtil.isBlank(segmentUserName) && StringUtil.isNotBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
      SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
      globalTraceIdAndTokenMap.put(globalTraceId, segmentToken);
      // 当用户名为null，但token和全局追踪id不为空；
      userName = globalTraceIdAndUserNameMap.get(globalTraceId);
      if (StringUtil.isNotBlank(userName)) {
        SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
        tokenAndUserNameMap.put(segmentToken, userName);
        segment.setUserName(userName);
      } else {
        userName = tokenAndUserNameMap.get(segmentToken);
        if (StringUtil.isNotBlank(userName)) {
          segment.setUserName(userName);
          SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
          globalTraceIdAndUserNameMap.put(globalTraceId, userName);
        } else {
          SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
          // try {
          //   tokenAndUserNameMap.put(segmentToken, null);
          //   globalTraceIdAndUserNameMap.put(globalTraceId, null);
          // } catch (Exception e) {
          //   e.printStackTrace();
          // }
        }
      }
    } else if (StringUtil.isNotBlank(segmentUserName) && StringUtil.isNotBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
      // 当用户名、token和全局追踪id都不为空；这时候，就可以把三个map补全了。2022-05-24 15:48:15
      SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
      globalTraceIdAndUserNameMap.put(globalTraceId, segmentUserName);
      globalTraceIdAndTokenMap.put(globalTraceId, segmentToken);
      tokenAndUserNameMap.put(segmentToken, segmentUserName);
    } else if (StringUtil.isNotBlank(segmentUserName) && StringUtil.isBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
      SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
      globalTraceIdAndUserNameMap.put(globalTraceId, segmentUserName);
    } else {
      System.out.println("出现异常情况了。用户名、token和全局追踪id都为空。");
    }
  }

  private void insertSegment(SegmentDo segment, SegmentObject segmentObject, String parentSegmentId) {
    if (segment.getOperationName().equals("Redisson/PING")) {
      return;
    }
    // 用户名和token都是空的调用链，不存入数据库中。这里只存入带有用户名或者token完整的调用链。2022-04-20 16:35:52
    if (StringUtil.isBlank(segment.getUserName()) && StringUtil.isBlank(segment.getToken())) {
      // log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，该调用链 = 【{}】 不含有用户名和token，不能插入到表中。", JsonUtil.obj2String(segment));
      return;
    } else if (StringUtil.isBlank(segment.getUserName()) && StringUtil.isNotBlank(segment.getToken())) {
      // 如果用户名为空，但token不为空，此时要把这个token对应的用户名补全；2022-04-21 08:45:30
      boolean flag = setUserNameByToken(segment);
      if (false == flag) {
        insertToken(segment);
      } else {
        insertUserNameAndTokenAndGlobalTraceId(segment);
      }
    } else if (StringUtil.isNotBlank(segment.getUserName()) && StringUtil.isNotBlank(segment.getToken())) {
      // 如果用户名和token都不为空，那么就把用户名和token插入到表中；2022-04-21 08:46:07
      insertUserNameAndToken(segment);
    }
    if (!StringUtil.isBlank(segment.getUserName())) {
      // 更新用户名为空的记录；2022-05-17 16:10:34
      updateUserName(segment);
    }

    // 将segment中缺少用户名的记录，补全用户名。之所以有这一步骤，是因为在分布式调用链中，每一部分都是独立上报信息的。如果后面的先上报，那么用户名和token很可能不会传递到后面。
    // 此时该条记录先插入到segment表中，用户名字段先置为null。当后面用户名不为空的记录到来时，再通过global_trace_id进行关联并补全上面缺失的用户名。
    // if (StringUtil.isBlank(segment.getUserName()) && !StringUtil.isBlank(segment.getGlobalTraceId())) {
    //   List<SegmentDo> segmentDoList = segmentDao.selectByGlobalTraceIdAndUserNameIsNull(segment.getGlobalTraceId());
    //   for (SegmentDo segmentDo : segmentDoList) {
    //     segmentDo.setUserName(segment.getUserName());
    //     int updateResult = segmentDao.updateByPrimaryKeySelective(segmentDo);
    //     if (1 != updateResult) {
    //       log.error("更新用户名 = [{}]失败。", segmentDo);
    //     }
    //   }
    // }

    int insertResult = segmentDao.insertSelective(segment);
    if (1 != insertResult) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，将完整的调用链 = 【{}】 插入到表中失败。", JsonUtil.obj2String(segment));
    } else {
      // 将全局 trace_id 和 segment_ids保存到表里，其目的是，将用户与这条访问链路上的各个segment绑定到一起；2022-04-24 17:32:06
      saveGlobalTraceIdAndSegmentIds(segmentObject, parentSegmentId, segment.getUserName(), segment.getToken());
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
    String token = segment.getToken();
    UserTokenDo userTokenDo = userTokenDao.selectByToken(token);
    if (null == userTokenDo) {
      String globalTraceId = segment.getGlobalTraceId();
      if (StringUtil.isNotBlank(globalTraceId)) {
        List<SegmentDo> segmentDoList = segmentDao.selectByGlobalTraceId(globalTraceId);
        if (null != segmentDoList && 0 < segmentDoList.size()) {
          for (SegmentDo segmentDo : segmentDoList) {
            if (StringUtil.isNotBlank(segmentDo.getUserName())) {
              segment.setUserName(segmentDo.getUserName());
              insertUserNameAndToken(segment);
              return true;
            }
          }
        }
      }
    } else {
      String userName = userTokenDo.getUserName();
      if (StringUtil.isNotBlank(userName)) {
        segment.setUserName(userName);
        return true;
      }
    }
    return false;
  }

  /**
   * <B>方法名称：insertUserNameAndToken</B>
   * <B>概要说明：将token信息插入到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年04月21日 08:04:30
   * @Param [segment]
   **/
  private void insertToken(SegmentDo segment) {
    UserTokenDo userTokenDo = new UserTokenDo();
    try {
      String token = segment.getToken();
      userTokenDo.setToken(token);
      String globalTraceId1 = segment.getGlobalTraceId();
      userTokenDo.setGlobalTraceId(globalTraceId1);
      String userName = segment.getUserName();
      UserTokenDo userTokenDo1 = null;
      if (StringUtil.isNotBlank(token) && StringUtil.isNotBlank(userName)) {
        userTokenDo1 = userTokenDao.selectByUserNameAndToken(userTokenDo);
      } else if (StringUtil.isNotBlank(token) && StringUtil.isBlank(userName)) {
        userTokenDo1 = userTokenDao.selectByToken(token);
      }

      if (null == userTokenDo1) {
        String globalTraceId = segment.getGlobalTraceId();
        if (StringUtil.isNotBlank(globalTraceId)) {
          UserTokenDo userTokenDo2 = userTokenDao.selectByGlobalTraceIdUserNameIsNotNull(globalTraceId);
          if (null != userTokenDo2) {
            userTokenDo.setUserName(userTokenDo2.getUserName());
          }
        }
        int insertResult = userTokenDao.insertSelective(userTokenDo);
        if (1 != insertResult) {
          log.error("开始执行 AiitKafkaConsumer # insertUserNameAndToken()方法，将用户名和token信息【{}】插入到表中失败。", JsonUtil.obj2String(userTokenDo));
        }
      }
    } catch (Exception e) {
      log.error("将用户的信息  = 【{}】插入到表中出现了异常。", JsonUtil.obj2String(userTokenDo), e);
    }
  }

  /**
   * <B>方法名称：updateUserNameAndToken</B>
   * <B>概要说明：更新用户名为空的记录</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年04月21日 08:04:30
   * @Param [segment]
   **/
  private void updateUserName(SegmentDo segment) {
    String userName = segment.getUserName();
    String token = segment.getToken();
    String globalTraceId = segment.getGlobalTraceId();
    try {
      if (StringUtil.isNotBlank(userName)) {
        List<UserTokenDo> userList = userTokenDao.selectByGlobalTraceIdUserNameIsNull(globalTraceId);
        if (null != userList && 0 < userList.size()) {
          // 根据全局追踪id找到了用户名为null的记录，此时将用户名补全；2022-05-17 16:27:55
          for (UserTokenDo tokenDo : userList) {
            tokenDo.setUserName(userName);
            int updateResult = userTokenDao.updateByPrimaryKeySelective(tokenDo);
            if (1 != updateResult) {
              log.error("将用户信息  = 【{}】更新到用户token表中失败。", JsonUtil.obj2String(tokenDo));
            }
          }
        }
        if (StringUtil.isNotBlank(token)) {
          List<UserTokenDo> tokenList = userTokenDao.selectByTokenUserNameIsNull(token);
          if (null != tokenList && 0 < tokenList.size()) {
            // 根据token找到了用户名为null的记录，此时将用户名补全；2022-05-17 16:27:55
            for (UserTokenDo tokenDo : tokenList) {
              tokenDo.setUserName(userName);
              int updateResult = userTokenDao.updateByPrimaryKeySelective(tokenDo);
              if (1 != updateResult) {
                log.error("将用户信息  = 【{}】更新到用户token表中失败。", JsonUtil.obj2String(tokenDo));
              }
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("根据全局追踪id  = 【{}】更新到用户名 = 【{}】时，出现了异常。", globalTraceId, userName, e);
    }
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
    try {
      userTokenDo.setUserName(segment.getUserName());
      userTokenDo.setToken(segment.getToken());
      userTokenDo.setGlobalTraceId(segment.getGlobalTraceId());
      UserTokenDo userTokenDo1 = userTokenDao.selectByUserNameAndToken(userTokenDo);
      if (null == userTokenDo1) {
        int insertResult = userTokenDao.insertSelective(userTokenDo);
        if (1 != insertResult) {
          log.error("开始执行 AiitKafkaConsumer # insertUserNameAndToken()方法，将用户名和token信息【{}】插入到表中失败。", JsonUtil.obj2String(userTokenDo));
        }
      }
    } catch (Exception e) {
      log.error("将用户的信息  = 【{}】插入到表中出现了异常。", JsonUtil.obj2String(userTokenDo), e);
    }
  }

  private void insertIndex() {
    try {
      Map<String, String> tokenAndUserNameMap = SingletonLocalStatisticsMap.getTokenAndUserNameMap();
      Map<String, String> globalTraceIdAndUserNameMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndUserNameMap();
      Iterator<String> iterator = globalTraceIdAndUserNameMap.keySet().iterator();
      while (iterator.hasNext()) {
        UserTokenDo userTokenDo = new UserTokenDo();
        String globalTraceId = iterator.next();
        String userName = globalTraceIdAndUserNameMap.get(globalTraceId);
        String token = MapUtil.getKey(tokenAndUserNameMap, userName);
        userTokenDo.setUserName(userName);
        userTokenDo.setToken(token);
        userTokenDo.setGlobalTraceId(globalTraceId);
        UserTokenDo userTokenDo1 = userTokenDao.selectByUserNameAndTokenAndGlobalTraceId(userTokenDo);
        if (null == userTokenDo1) {
          int insertResult = userTokenDao.insertSelective(userTokenDo);
          if (1 != insertResult) {
            log.error("开始执行 AiitKafkaConsumer # insertUserNameAndToken()方法，将用户名和token信息【{}】插入到表中失败。", JsonUtil.obj2String(userTokenDo));
          }
        }
      }
    } catch (Exception e) {
      log.error("将segment的索引信息插入到表中出现了异常。", e);
    }
  }

  private void insertUserNameAndTokenAndGlobalTraceId(SegmentDo segment) {
    UserTokenDo userTokenDo = new UserTokenDo();
    try {
      userTokenDo.setUserName(segment.getUserName());
      userTokenDo.setToken(segment.getToken());
      userTokenDo.setGlobalTraceId(segment.getGlobalTraceId());
      UserTokenDo userTokenDo1 = userTokenDao.selectByUserNameAndTokenAndGlobalTraceId(userTokenDo);
      if (null == userTokenDo1) {
        int insertResult = userTokenDao.insertSelective(userTokenDo);
        if (1 != insertResult) {
          log.error("开始执行 AiitKafkaConsumer # insertUserNameAndToken()方法，将用户名和token信息【{}】插入到表中失败。", JsonUtil.obj2String(userTokenDo));
        }
      } else {
        // log.info("根据用户名 = 【{}】和token = 【{}】在表里找到了记录。", segment.getUserName(), segment.getToken());
      }
    } catch (Exception e) {
      log.error("将用户的信息  = 【{}】插入到表中出现了异常。", JsonUtil.obj2String(userTokenDo), e);
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
      if (!StringUtil.isBlank(component)) {
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
      // 为了制造千万级的数据，这里暂时不存储span的信息；2022-05-19 08:43:32
      // segment.setSpans(JsonUtil.obj2String(spanList));
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
    } catch (Exception e) {
      log.error("将span对应的二进制类型的数据转换成字符串类型的数据时，出现了异常。", e);
    }
    return segment;
  }
}
