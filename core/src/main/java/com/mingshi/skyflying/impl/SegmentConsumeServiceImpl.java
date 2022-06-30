package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionUtil;
import com.mingshi.skyflying.anomaly_detection.singleton.StatisticsConsumeProcessorThreadQPS;
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
import java.time.Instant;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;

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
  private MingshiServerUtil mingshiServerUtil;
  @Resource
  private SegmentDao segmentDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;
  @Resource
  private UserTokenDao userTokenDao;

  private AtomicInteger atomicInteger = new AtomicInteger(0);

  @Override
  public ServerResponse<String> consume(ConsumerRecord<String, Bytes> record, Boolean enableReactorModelFlag) {
    doConsume(record, enableReactorModelFlag);
    // 统计QPS；2022-06-24 10:34:24
    StatisticsConsumeProcessorThreadQPS.accumulateTimes(Thread.currentThread().getName(), DateTimeUtil.dateToStrformat(new Date()));
    return null;
  }

  private void doConsume(ConsumerRecord<String, Bytes> record, Boolean enableReactorModelFlag) {
    Instant now1 = Instant.now();
    Instant now = Instant.now();
    SegmentObject segmentObject = null;
    Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
    try {
      segmentObject = SegmentObject.parseFrom(record.value().get());

      SegmentDo segment = null;
      // 暂存sql语句的来源：skywalking 探针；2022-05-27 18:36:50
      LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgentList = null;
      // 判断是否是异常信息；2022-06-07 18:00:13
      LinkedList<MsAlarmInformationDo> msAlarmInformationDoList = null;
      // 将一条访问操作过程中涉及到的多条SQL语句拆成一条一条的SQL；2022-06-09 08:55:18
      LinkedList<MsSegmentDetailDo> segmentDetaiDolList = null;
        // 获取探针的名称；2022-06-28 14:25:46
      skywalkingAgentHeartBeatMap = getAgentServiceName(segmentObject);

      List<Span> spanList = buildSpanList(segmentObject);
      if(null != spanList && 0 < spanList.size()){
        // 组装segment；2022-04-20 16:33:48
        segment = setUserNameAndToken(spanList);

        // 设置segment_id、trace_id；2022-04-24 14:26:12
        getRef(segmentObject, segment);

        setSegmentIndex(segment);

        auditLogFromSkywalkingAgentList = new LinkedList<>();
        // 重组span数据，返回前端使用；2022-04-20 16:49:02
        reorganizingSpans(segment, spanList, auditLogFromSkywalkingAgentList);

        // log.info(" # SegmentConsumeServiceImpl.doConsume() # 执行完95行，用时【{}】毫秒。",DateTimeUtil.getTimeMillis(now));
        // now = Instant.now();

        // 将一条访问操作过程中涉及到的多条SQL语句拆成一条一条的SQL；2022-06-09 08:55:18
        segmentDetaiDolList = getSegmentDetaiDolList(segment);
        // log.info(" # SegmentConsumeServiceImpl.doConsume() # 执行完100行，用时【{}】毫秒。",DateTimeUtil.getTimeMillis(now));

        // 判断是否是异常信息；2022-06-07 18:00:13
        msAlarmInformationDoList = new LinkedList<>();

        AnomalyDetectionUtil.userVisitedTimeIsAbnormal(segment, msAlarmInformationDoList);
        AnomalyDetectionUtil.userVisitedTableIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
      }

      // 将组装好的segment插入到表中；2022-04-20 16:34:01
      if (true == enableReactorModelFlag) {
        // 使用reactor模型；2022-05-30 21:04:05
        doEnableReactorModel(segment, auditLogFromSkywalkingAgentList, segmentDetaiDolList, msAlarmInformationDoList, skywalkingAgentHeartBeatMap);
      } else {

        // 将QPS信息刷入Redis中；2022-06-27 13:42:13
        // mingshiServerUtil.flushQpsToRedis();

        // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
        mingshiServerUtil.flushSkywalkingAgentInformationToDb();

        // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
        mingshiServerUtil.flushSkywalkingAgentInformationToDb();

        // 将探针名称发送到Redis中，用于心跳检测；2022-06-27 13:42:13
        mingshiServerUtil.flushSkywalkingAgentNameToRedis(skywalkingAgentHeartBeatMap);

        // 不使用reactor模型；2022-05-30 21:04:16
        // 插入segment数据；2022-05-23 10:15:22
        // LinkedList<SegmentDo> segmentDoLinkedList = new LinkedList<>();
        // segmentDoLinkedList.add(segment);
        // mingshiServerUtil.flushSegmentToDB(segmentDoLinkedList);

        // 插入segment对应的index数据；2022-05-23 10:15:38
        mingshiServerUtil.updateUserNameByGlobalTraceId();

        // mingshiServerUtil.flushAuditLogToDB(auditLogFromSkywalkingAgentList);
        // 将segmentDetail实例信息插入到数据库中；2022-06-02 11:07:51
        mingshiServerUtil.flushSegmentDetailToDB(segmentDetaiDolList);
        // 将异常信息插入到MySQL中；2022-06-07 18:16:44
        LinkedList<MsAlarmInformationDo> msAlarmInformationDoLinkedListist = new LinkedList<>();
        if(null != msAlarmInformationDoList && 0 < msAlarmInformationDoList.size()){
          msAlarmInformationDoLinkedListist.addAll(msAlarmInformationDoList);
        }
        mingshiServerUtil.flushAbnormalToDB(msAlarmInformationDoLinkedListist);
      }
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
    // log.info(" # SegmentConsumeServiceImpl.doConsume() # 消费完一条链路信息用时【{}】毫秒。",DateTimeUtil.getTimeMillis(now1));
  }

  /**
   * <B>方法名称：getAgentServiceName</B>
   * <B>概要说明：获取探针的名称</B>
   *
   * @return java.util.Map<java.lang.String, java.lang.String>
   * @Author zm
   * @Date 2022年06月28日 14:06:38
   * @Param [segmentObject]
   **/
  private Map<String, String> getAgentServiceName(SegmentObject segmentObject) {
    Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
    try {
      skywalkingAgentHeartBeatMap = new HashMap<>();
      String service = segmentObject.getService();
      String serviceInstance = segmentObject.getServiceInstance();
      ObjectNode jsonObject = JsonUtil.createJSONObject();
      jsonObject.put("serviceCode", service);
      jsonObject.put("serviceInstanceName", serviceInstance);
      skywalkingAgentHeartBeatMap.put(jsonObject.toString(), DateTimeUtil.DateToStr(new Date()));
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.getAgentServiceName() # 获取探针的名称时，出现了异常。", e);
    }
    return skywalkingAgentHeartBeatMap;
  }

  private LinkedList<MsSegmentDetailDo> getSegmentDetaiDolList(SegmentDo segment) {
    LinkedList<MsSegmentDetailDo> segmentDetaiDolList = null;
    try {
      segmentDetaiDolList = new LinkedList<>();
      String reorganizingSpans = segment.getReorganizingSpans();
      if (StringUtil.isBlank(reorganizingSpans)) {
        return segmentDetaiDolList;
      }
      List<LinkedHashMap> list = JsonUtil.string2Obj(reorganizingSpans, List.class, LinkedHashMap.class);
      if (null == list || 0 == list.size()) {
        return segmentDetaiDolList;
      }
      LinkedHashMap map1 = list.get(0);
      Object url = map1.get("url");
      MsSegmentDetailDo msSegmentDetailDo = null;
      for (int i = 1; i < list.size(); i++) {
        msSegmentDetailDo = new MsSegmentDetailDo();
        msSegmentDetailDo.setUserPortraitFlagByVisitedTime(null == segment.getUserPortraitFlagByVisitedTime() ? 0 : segment.getUserPortraitFlagByVisitedTime());
        LinkedHashMap map = list.get(i);
        msSegmentDetailDo.setOperationName(String.valueOf(url));

        Integer spanId = null;
        if (null != map.get("spanId")) {
          spanId = Integer.valueOf(String.valueOf(map.get("spanId")));
        }

        String component = String.valueOf(map.get("component"));
        String serviceCode = String.valueOf(map.get("serviceCode"));
        String peer = String.valueOf(map.get("peer"));
        String endpointName = String.valueOf(map.get("endpointName"));
        Long startTime = Long.valueOf(String.valueOf(map.get("startTime")));
        String serviceInstanceName = String.valueOf(map.get("serviceInstanceName"));
        Long endTime = Long.valueOf(String.valueOf(map.get("endTime")));
        Integer parentSpanId = Integer.valueOf(String.valueOf(map.get("parentSpanId")));
        String tags = String.valueOf(map.get("tags"));

        List<KeyValue> tagsList = JsonUtil.string2Obj(tags, List.class, KeyValue.class);
        if (null != tagsList) {
          String isSql = null;
          for (KeyValue keyValue : tagsList) {
            String key = keyValue.getKey();
            String value = keyValue.getValue();
            if (key.equals("db.type")) {
              isSql = value;
              msSegmentDetailDo.setOperationType(value);
            } else if (key.equals("db.instance")) {
              msSegmentDetailDo.setDbInstance(value);
            } else if (key.equals("db_user_name")) {
              msSegmentDetailDo.setDbUserName(value);
            } else if (key.equals("db.statement")) {
              if (isSql.equals("sql")) {
                // 获取表名；2022-06-06 14:16:59
                setTableName(value, msSegmentDetailDo);
              }
              msSegmentDetailDo.setDbStatement(value);
            } else if (key.equals("url")) {
              msSegmentDetailDo.setDbType("url");
              msSegmentDetailDo.setDbStatement(value);
            }
          }
        }

        msSegmentDetailDo.setToken(segment.getToken());
        msSegmentDetailDo.setComponent(component);
        msSegmentDetailDo.setSpanId(spanId);
        msSegmentDetailDo.setServiceCode(serviceCode);
        msSegmentDetailDo.setPeer(peer);
        msSegmentDetailDo.setEndpointName(endpointName);
        msSegmentDetailDo.setStartTime(DateTimeUtil.longToDate(startTime));
        msSegmentDetailDo.setServiceInstanceName(serviceInstanceName);
        msSegmentDetailDo.setEndTime(DateTimeUtil.longToDate(endTime));
        msSegmentDetailDo.setParentSpanId(parentSpanId);
        msSegmentDetailDo.setUserName(segment.getUserName());
        msSegmentDetailDo.setGlobalTraceId(segment.getGlobalTraceId());
        msSegmentDetailDo.setParentSegmentId(segment.getParentSegmentId());
        msSegmentDetailDo.setCurrentSegmentId(segment.getCurrentSegmentId());
        segmentDetaiDolList.add(msSegmentDetailDo);
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.getSegmentDetaiDolList() # 组装segmentDetail详情实例时，出现了异常。", e);
    }
    return segmentDetaiDolList;
  }

  /**
   * <B>方法名称：setTableName</B>
   * <B>概要说明： 根据sql语句获取表名</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月06日 14:06:09
   * @Param [value, msSegmentDetailDo]
   **/
  private void setTableName(String value, MsSegmentDetailDo msSegmentDetailDo) {
    try {
      // sql类型；
      String sqlType = mingshiServerUtil.getSqlType(value);
      msSegmentDetailDo.setDbType(sqlType);
      // 获取表名；2022-06-06 14:11:21
      String tableName = mingshiServerUtil.getTableName(sqlType, value);
      msSegmentDetailDo.setMsTableName(tableName);
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.setTableName() # 根据sql语句获取表名时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：ignoreMethod</B>
   * <B>概要说明：判断是否可以过滤掉</B>
   *
   * @return java.lang.Boolean
   * @Author zm
   * @Date 2022年05月30日 20:05:06
   * @Param [operationName]
   **/
  private Boolean ignoreMethod(String operationName) {
    // TODO: 2022/6/28 正常来说，需要忽略的操作类型应该配置到数据库中或者配置到配置文件中，不应该写死在这里。
    if (operationName.equals("Redisson/PING") ||
      operationName.equals("Jedis/sentinelGetMasterAddrByName") ||
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
      return true;
    }
    return false;
  }

  private void doEnableReactorModel(SegmentDo segmentDo,
                                    LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgentList,
                                    LinkedList<MsSegmentDetailDo> segmentDetaiDolList,
                                    LinkedList<MsAlarmInformationDo> msAlarmInformationDoList,
                                    Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap) {
    try {
      LinkedBlockingQueue linkedBlockingQueue = BatchInsertByLinkedBlockingQueue.getLinkedBlockingQueue(1, 5, mingshiServerUtil);
      ObjectNode jsonObject = JsonUtil.createJSONObject();
      if (null != segmentDo) {
        jsonObject.put(Const.SEGMENT_LIST, JsonUtil.obj2String(segmentDo));
      }
      if (null != auditLogFromSkywalkingAgentList && 0 < auditLogFromSkywalkingAgentList.size()) {
        jsonObject.put(Const.AUDITLOG_FROM_SKYWALKING_AGENT_LIST, JsonUtil.obj2String(auditLogFromSkywalkingAgentList));
      }
      if (null != segmentDetaiDolList && 0 < segmentDetaiDolList.size()) {
        jsonObject.put(Const.SEGMENT_DETAIL_DO_LIST, JsonUtil.obj2String(segmentDetaiDolList));
      }
      if (null != msAlarmInformationDoList && 0 < msAlarmInformationDoList.size()) {
        jsonObject.put(Const.ABNORMAL, JsonUtil.obj2String(msAlarmInformationDoList));
      }
      if (null != skywalkingAgentHeartBeatMap && 0 < skywalkingAgentHeartBeatMap.size()) {
        jsonObject.put(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, JsonUtil.obj2String(skywalkingAgentHeartBeatMap));
      }
      if (linkedBlockingQueue.size() == BatchInsertByLinkedBlockingQueue.getQueueSize()) {
        // 每200条消息打印一次日志，否则会影响系统性能；2022-01-14 10:57:15
        log.info("将调用链信息放入到BatchInsertByLinkedBlockingQueue队列中，队列满了，当前队列中的元素个数【{}】，队列的容量【{}】。", linkedBlockingQueue.size(), BatchInsertByLinkedBlockingQueue.getQueueSize());
      }
      // if (0 == atomicInteger.incrementAndGet() % 5000) {
      //   // 每200条消息打印一次日志，否则会影响系统性能；2022-01-14 10:57:15
      //   log.info("将调用链信息放入到BatchInsertByLinkedBlockingQueue队列中，当前队列中的元素个数【{}】，队列的容量【{}】。", linkedBlockingQueue.size(), BatchInsertByLinkedBlockingQueue.getQueueSize());
      // }
      if (null != jsonObject && 0 < jsonObject.size()) {
        linkedBlockingQueue.put(jsonObject);
      }
    } catch (Exception e) {
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
        ObjectNode jsonObject = JsonUtil.createJSONObject();
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
          linkedHashSet.add(jsonObject.toString());
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
          linkedHashSet.add(jsonObject.toString());
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

  private void reorganizingSpans(SegmentDo segment, List<Span> spanList, LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgent) {
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
        getData2(segment, span, linkedList, auditLogFromSkywalkingAgent);
        findChildrenDetail(segment, spanList, span, childrenSpan, linkedList, auditLogFromSkywalkingAgent);
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

  private void findChildrenDetail(SegmentDo segmentDo, List<Span> spans, Span parentSpan, List<Span> childrenSpan, List<String> linkedList, LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgent) {
    spans.forEach(span -> {
      if (span.getSegmentParentSpanId().equals(parentSpan.getSegmentSpanId())) {
        childrenSpan.add(span);
        getData2(segmentDo, span, linkedList, auditLogFromSkywalkingAgent);
        findChildrenDetail(segmentDo, spans, span, childrenSpan, linkedList, auditLogFromSkywalkingAgent);
      }
    });
  }

  /**
   * <B>方法名称：getData2</B>
   * <B>概要说明：只要span中的tags字段不为空，那么就把这个span放入到链表中。这样做的目的是：不再区分是哪个插件拦截到的信息，像skywalking的服务端一样，使用统一的展示方式在前端展示数据。</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月17日 10:05:41
   * @Param [span, linkedList]
   **/
  private void getData2(SegmentDo segmentDo, Span span, List<String> linkedList, LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgent) {
    try {
      ObjectNode jsonObject = JsonUtil.createJSONObject();
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
        List<KeyValue> tags = span.getTags();
        if (0 < tags.size()) {
          Boolean flag = false;
          Boolean isSQL = false;
          String dbUserName = null;
          String msSql = null;
          String msSchemaName = null;
          String key = null;
          for (KeyValue tag : tags) {
            key = tag.getKey();
            if (segmentDo.getOperationName().equals("Jedis/sentinelGetMasterAddrByName")) {
              flag = true;
              break;
            }
            if (tag.getValue().equals("Redis")) {
              // 不再存储单纯的Redis请求；2022-06-30 16:34:24
              flag = true;
              break;
            }
            if (key.equals("http.method")) {
              // 不再存储单纯的GET请求；2022-05-27 18:14:25
              flag = true;
              break;
            } else if (key.equals("db.instance")) {
              msSchemaName = tag.getValue();
            } else if (key.equals("db_user_name")) {
              dbUserName = tag.getValue();
            } else if (key.equals("db.statement") && !key.equals("Redis")) {
              // 一开始的想法：这里需要对SQL语句进行规范化，否则无法将探针获取到的SQL与阿里云的SQL洞察获取到的SQL进行精确匹配；2022-05-27 21:12:13
              // 想法更改：这里不需要对SQL语句进行格式化了，因为skywalking的Java探针截取到的SQL语句有一定的格式，一般人很难在Navicat这样的工具中，来模仿Java探针的SQL语句格式。通过这个格式就可以简单区分来自SQL洞察中的skywalking探针发出的SQL；2022-05-28 12:48:12
              msSql = tag.getValue();
              isSQL = true;
            }
          }
          if (true == isSQL && StringUtil.isNotBlank(msSql)) {
            // 将SQL组装成对象，并放入到list集合中；2022-05-28 13:22:45
            getMsAuditLogDo(segmentDo, msSql, span, msSchemaName, dbUserName, auditLogFromSkywalkingAgent);
          }

          if (false == flag) {
            jsonObject.put("tags", JsonUtil.obj2String(tags));
            linkedList.add(jsonObject.toString());
          }
        }
      }
    } catch (Exception e) {
      log.error("将span的信息 = 【{}】放入到LinkedList中的时候，出现了异常。", JsonUtil.obj2StringPretty(span), e);
    }
  }

  /**
   * <B>方法名称：getMsAuditLogDo</B>
   * <B>概要说明：组装MsAuditLogDo实例，用于将其插入到审计日志表中（ms_audit_log）</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月28日 13:05:03
   * @Param [msSql, startTime, msSchemaName, dbUserName, auditLogFromSkywalkingAgent]
   **/
  private void getMsAuditLogDo(SegmentDo segmentDo, String msSql, Span span, String msSchemaName, String dbUserName, LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgent) {
    MsAuditLogDo msAuditLogDo = new MsAuditLogDo();
    try {
      msAuditLogDo.setApplicationUserName(segmentDo.getUserName());
      msAuditLogDo.setOperationName(segmentDo.getOperationName());
      msAuditLogDo.setCurrentSegmentId(span.getSegmentId());
      msAuditLogDo.setParentSegmentId(segmentDo.getParentSegmentId());
      // 设置全局追踪id；2022-05-30 18:55:00
      String traceId = span.getTraceId();
      msAuditLogDo.setGlobalTraceId(traceId);
      // 设置登录应用系统的用户名；2022-05-30 18:55:12

      msAuditLogDo.setSqlSource(Const.SQL_SOURCE_SKYWALKING_AGENT);
      // sql语句；
      msAuditLogDo.setMsSql(msSql);
      // 执行时间；
      long startTime = span.getStartTime();
      String opTime = DateTimeUtil.longToDate(startTime);
      msAuditLogDo.setOpTime(opTime);
      // 数据库名称；
      msAuditLogDo.setMsSchemaName(msSchemaName);
      // 执行语句的数据库用户名；
      msAuditLogDo.setSqlInsightDbUserName(dbUserName);
      // 发送请求的客户端IP；
      // msAuditLogDo.setSqlInsightUserIp(data.getUSER_IP());
      // sql类型；
      String sqlType = mingshiServerUtil.getSqlType(msSql);
      msAuditLogDo.setSqlType(sqlType);

      // 获取表名；2022-05-31 17:01:39
      String tableName = mingshiServerUtil.getTableName(sqlType, msSql);
      msAuditLogDo.setMsTableName(tableName);

      // 这个来自探针的操作时间opTime不是SQL语句真正的执行时间，所以这里就不传了。直接根据sql语句 + 数据库名称 + sql类型来计算md5值；2022-05-28 13:09:47
      // 这里有也有一个问题：当来自探针的同一条SQL在不同的时间过来时，会根据hash值进行更新。为了解决这个问题，数据库中ms_audit_log中的hash字段就不能设置为唯一索引了。
      // 当初设置这个hash字段为唯一索引时，是为了识别出来自SQL洞察中skywalking探针发出来的SQL语句。
      // 由于SQL洞察中的数据量巨大，且处理出来还比较麻烦。所以李老师就不打算处理SQL洞察中的数据了。进而可以将数据库表ms_audit_log中的hash字段不再设置为唯一索引。
      // 2022-06-01 15:43:56
      String strData = StringUtil.recombination(msSql, null, msSchemaName, sqlType);
      String hash = StringUtil.MD5(strData);
      msAuditLogDo.setHash(hash);
      auditLogFromSkywalkingAgent.add(msAuditLogDo);
    } catch (Exception e) {
      log.error("#SegmentConsumeServiceImpl.getMsAuditLogDo()# 组装MsAuditLogDo实例时，出现了异常。", JsonUtil.obj2String(msAuditLogDo));
    }
  }

  /**
   * <B>方法名称：getSpringMVCInfo</B>
   * <B>概要说明：获取SpringMVC信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月12日 16:05:24
   * @Param [span, jsonObject, linkedList]
   **/
  private void getSpringMVCInfo(Span span, ObjectNode jsonObject, List<String> linkedList) {
    List<KeyValue> tagsList = span.getTags();
    for (KeyValue keyValue : tagsList) {
      if (keyValue.getKey().equals("url")) {
        String url = keyValue.getValue();
        jsonObject.put("url", url);
        linkedList.add(jsonObject.toString());
      }
    }
  }

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
          SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(true);
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
      String operationName = spanObject.getOperationName();

      // 忽略掉不需要的链路信息；2022-06-28 14:18:20
      Boolean flag = ignoreMethod(operationName);
      if (true == flag) {
        continue;
      }

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

      span.setEndpointName(operationName);

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
