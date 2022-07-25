package com.mingshi.skyflying.disruptor.iothread;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.WorkHandler;
import com.mingshi.skyflying.agent.AgentInformationSingleton;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.elasticsearch.domain.EsMsSegmentDetailDo;
import com.mingshi.skyflying.elasticsearch.utils.EsMsSegmentDetailUtil;
import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.statistics.InformationOverviewSingleton;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.util.*;

/**
 * @Author zhaoming
 * @Description 每一个线程从队列中获取匹配后的数据，然后在本地统计；当超过了一定时间后，再统一发送到Redis中；
 * @Date 18:15 2021/10/13
 * @Param
 * @return
 **/
@Slf4j
public class IoThreadByEventHandler implements WorkHandler<IoThreadObjectNode>{

  private Instant CURRENT_TIME = null;
  private Integer flushToRocketMQInterval = 10;
  private Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
  private Map<String/* 线程名称 */, Map<String/* 时间 */,Integer/* 消费的数量 */>> processorThreadQpsMap = null;
  private HashSet<Map<String/* 时间 */,Integer/* 队列的大小 */>> ioThreadQueueSet = null;
  private LinkedList<SegmentDo> segmentList = null;
  private LinkedList<MsAuditLogDo> auditLogList = null;
  private LinkedList<MsSegmentDetailDo> segmentDetailDoList = null;
  private HashSet<String> userHashSet = null;
  private LinkedList<Span> spanList = null;
  private LinkedList<EsMsSegmentDetailDo> esSegmentDetailDoList = null;
  private List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist = null;
  private MingshiServerUtil mingshiServerUtil;
  private EsMsSegmentDetailUtil esMsSegmentDetailUtil;

  private RingBuffer<IoThreadObjectNode> ringBuffer;

  public IoThreadByEventHandler(Integer flushToRocketMQInterval, MingshiServerUtil mingshiServerUtil, EsMsSegmentDetailUtil esMsSegmentDetailUtil, RingBuffer<IoThreadObjectNode> ringBuffer) {
    CURRENT_TIME = Instant.now().minusSeconds(new Random().nextInt(30));
    // 懒汉模式：只有用到的时候，才创建list实例。2022-06-01 10:22:16
    skywalkingAgentHeartBeatMap = new HashMap<>();
    processorThreadQpsMap = new HashMap<>();
    ioThreadQueueSet = new HashSet<>();
    segmentList = new LinkedList();
    userHashSet = new HashSet();
    auditLogList = new LinkedList();
    segmentDetailDoList = new LinkedList();
    spanList = new LinkedList();
    esSegmentDetailDoList = new LinkedList();
    msAlarmInformationDoLinkedListist = new LinkedList();
    // 防御性编程，当间隔为null或者小于0时，设置成5；2022-05-19 18:11:31
    if (null == flushToRocketMQInterval || flushToRocketMQInterval < 0) {
      this.flushToRocketMQInterval = 5;
    }
    this.mingshiServerUtil = mingshiServerUtil;
    this.esSegmentDetailDoList = esSegmentDetailDoList;
    this.esMsSegmentDetailUtil = esMsSegmentDetailUtil;
    this.ringBuffer = ringBuffer;
  }

  // 单线程的情况下使用；2021-12-23 07:53:11
  @Override
  public void onEvent(IoThreadObjectNode ioThreadObjectNode) {
    try {
      ObjectNode jsonObject = ioThreadObjectNode.getData();
      // 从json实例中获取探针名称信息，用于心跳；2022-06-27 13:40:44
      getSkywalkingAgentNameFromJSONObject(jsonObject);

      // 统计processorThread线程的QPS；2022-07-23 11:15:29
      getProcessorThreadQpsFromJSONObject(jsonObject);

      getIoThreadQueueFromJSONObject(jsonObject);

      // 从json实例中获取segmentDetail实例的信息
      getSegmentDetailFromJSONObject(jsonObject);

      // 从json实例中获取esSegmentDetail实例的信息
      getEsSegmentDetailFromJSONObject(jsonObject);

      // 从json实例中获取Span实例的信息
      // getSpanFromJSONObject(jsonObject);

      // 从json实例中获取异常信息
      getAbnormalFromJSONObject(jsonObject);

      // 从json实例中获取审计日志的信息
      // getAuditLogFromJSONObject(jsonObject);

      // 从json实例中获取segment的信息
      // getSegmentFromJSONObject(jsonObject);

      // 将segment信息和SQL审计日志插入到表中；2022-05-30 17:50:12
      insertSegmentAndIndexAndAuditLog();
    } catch (Throwable e) {
      log.error("# IoThreadByEventHandler.run() # 将segment信息、及对应的索引信息和SQL审计日志信息在本地攒批和批量插入时 ，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getAbnormalFromJSONObject</B>
   * <B>概要说明：将异常信息信息放入到 msAlarmInformationDoLinkedListist 中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月02日 11:06:40
   * @Param [jsonObject]
   **/
  private void getAbnormalFromJSONObject(ObjectNode jsonObject) {
    try {
      JsonNode jsonNode = jsonObject.get(Const.ABNORMAL);
      if (null != jsonNode) {
        String listString = jsonNode.asText();
        if (StringUtil.isNotBlank(listString)) {
          LinkedList<MsAlarmInformationDo> msAlarmInformationDoList = JsonUtil.string2Obj(listString, LinkedList.class, MsAlarmInformationDo.class);
          msAlarmInformationDoLinkedListist.addAll(msAlarmInformationDoList);
        }
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getAbnormalFromJSONObject() # 将异常信息放入到 msAlarmInformationDoLinkedListist 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getSkywalkingAgentNameFromJSONObject</B>
   * <B>概要说明：获取探针名称，用于更新探针心跳</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月27日 13:06:49
   * @Param [jsonObject]
   **/
  private void getSkywalkingAgentNameFromJSONObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      JsonNode jsonNode = jsonObject.get(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST);
      if (null != jsonNode) {
        listString = jsonNode.asText();
      }
      if (StringUtil.isNotBlank(listString)) {
        Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentTimeMap = JsonUtil.string2Obj(listString, Map.class);
        Set<String> stringSet = skywalkingAgentTimeMap.keySet();
        for (String set : stringSet) {
          Map<String, String> map = JsonUtil.string2Obj(set, Map.class);
          String serviceCode = map.get("serviceCode");
          AgentInformationSingleton.put(serviceCode, Const.DOLLAR);
        }
        skywalkingAgentHeartBeatMap.putAll(skywalkingAgentTimeMap);
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getProcessorThreadQpsFromJSONObject</B>
   * <B>概要说明：获取QPS数据</B>
   * @Author zm
   * @Date 2022年07月23日 11:07:40
   * @Param [jsonObject]
   * @return void
   **/
  private void getProcessorThreadQpsFromJSONObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      JsonNode jsonNode = jsonObject.get(Const.ZSET_PROCESSOR_THREAD_QPS);
      if (null != jsonNode) {
        listString = jsonNode.asText();
      }
      if (StringUtil.isNotBlank(listString)) {
        HashMap<String, Map<String, Integer>> map = JsonUtil.string2Obj(listString, HashMap.class);
        if(0 == processorThreadQpsMap.size()){
          if(null != map && 0 < map.size()){
            processorThreadQpsMap.putAll(map);
          }
        }else{
          Iterator<String> iterator = map.keySet().iterator();
          while(iterator.hasNext()){
            String threadName = iterator.next();
            Map<String, Integer> timeCountMap = map.get(threadName);
            if(null == timeCountMap || 0 == timeCountMap.size()){
              continue;
            }
            Map<String, Integer> map1 = processorThreadQpsMap.get(threadName);
            if(null == map1){
              processorThreadQpsMap.put(threadName,timeCountMap);
              continue;
            }

            Iterator<String> iterator1 = timeCountMap.keySet().iterator();
            while(iterator1.hasNext()){
              String time = iterator1.next();
              Integer count = timeCountMap.get(time);
              Integer integer = map1.get(time);
              if(null == integer){
                map1.put(time,count);
              }else{
                map1.put(time,count + integer);
              }
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
    }
  }

  private void getIoThreadQueueFromJSONObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      JsonNode jsonNode = jsonObject.get(Const.ZSET_IO_THREAD_QUEUE_SIZE);
      if (null != jsonNode) {
        listString = jsonNode.asText();
      }
      if (StringUtil.isNotBlank(listString)) {
        HashMap<String, Integer> map = JsonUtil.string2Obj(listString, HashMap.class);
        if(null != map && 0 < map.size()){
          ioThreadQueueSet.add(map);
        }
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getEsSegmentDetailFromJSONObject</B>
   * <B>概要说明：将EsSegmentDetail实例信息放入到 esSegmentDetailList 中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月13日 08:07:22
   * @Param [jsonObject]
   **/
  private void getEsSegmentDetailFromJSONObject(ObjectNode jsonObject) {
    if(null == esMsSegmentDetailUtil && false == esMsSegmentDetailUtil.getEsEnable()){
      return;
    }
    try {
      String listString = null;
      try {
        JsonNode jsonNode = jsonObject.get(Const.ES_SEGMENT_DETAIL_DO_LIST);
        if (null != jsonNode) {
          listString = jsonNode.asText();
        }
      } catch (Exception e) {
        log.error("# IoThreadByEventHandler.getEsSegmentDetailFromJSONObject() # 将EsEegmentDetail实例信息放入到 esSegmentDetailList 中出现了异常。", e);
      }
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<EsMsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, EsMsSegmentDetailDo.class);
        esSegmentDetailDoList.addAll(segmentDetailList);
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getEsSegmentDetailFromJSONObject() # 将EsSegmentDetail实例信息放入到 esSegmentDetailList 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getSegmentDetailFromJSONObject</B>
   * <B>概要说明：将segmentDetail实例信息放入到 segmentDetailList 中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月02日 11:06:40
   * @Param [jsonObject]
   **/
  private void getSpanFromJSONObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      try {
        JsonNode jsonNode = jsonObject.get(Const.SPAN);
        if (null != jsonNode) {
          listString = jsonNode.asText();
        }
      } catch (Exception e) {
        log.error("# IoThreadByEventHandler.getSpanFromJSONObject() # 将Span实例信息放入到 spanList 中出现了异常。", e);
      }
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<Span> spanLinkedList = JsonUtil.string2Obj(listString, LinkedList.class, Span.class);
        spanList.addAll(spanLinkedList);
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getSpanFromJSONObject() # 将Span实例信息放入到 spanList 中出现了异常。", e);
    }
  }
  private void getSegmentDetailFromJSONObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      try {
        JsonNode jsonNode = jsonObject.get(Const.SEGMENT_DETAIL_DO_LIST);
        if (null != jsonNode) {
          listString = jsonNode.asText();
        }
      } catch (Exception e) {
        log.error("# IoThreadByEventHandler.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
      }
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<MsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, MsSegmentDetailDo.class);
        // 之所将用户名判断放到这里来判断，而不是放到processor线程中判断，原因是为了提高效率。
        // 如果要放到processor中进行判断，如果该用户不存在，那么需要将其保存到Redis中。
        // 在保存到Redis中的时候，需要将其加锁。一旦在processor中加锁，就降低了processor线程的性能。
        for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailList) {
          String userName = msSegmentDetailDo.getUserName();
          String tableName = msSegmentDetailDo.getMsTableName();
          if(StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(tableName)){
            // 判断用户是否已存在，如果不存在，那么先暂存起来，然后再将其发送到Redis中；2022-07-19 10:03:22
            Boolean userIsExisted = InformationOverviewSingleton.userIsExisted(userName);
            if(false == userIsExisted){
              userHashSet.add(userName);
            }
          }
        }
        segmentDetailDoList.addAll(segmentDetailList);
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getAuditLogFromJSONObject</B>
   * <B>概要说明：从json实例中获取审计日志的信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 10:06:21
   * @Param [jsonObject]
   **/
  // private void getAuditLogFromJSONObject(ObjectNode jsonObject) {
  //   try {
  //     String listString = jsonObject.get(Const.AUDITLOG_FROM_SKYWALKING_AGENT_LIST).asText();
  //     if (StringUtil.isNotBlank(listString)) {
  //       LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgentList = JsonUtil.string2Obj(listString, LinkedList.class, MsAuditLogDo.class);
  //       auditLogList.addAll(auditLogFromSkywalkingAgentList);
  //     }
  //   } catch (Exception e) {
  //     log.error("# IoThreadByEventHandler.getAuditLogFromJSONObject() # 将来自skywalking探针的审计日志放入到 auditLogList 表中出现了异常。", e);
  //   }
  // }

  /**
   * <B>方法名称：getSegmentFromJSONObject</B>
   * <B>概要说明：从json实例中获取segment的信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 10:06:21
   * @Param [jsonObject]
   **/
  private void getSegmentFromJSONObject(ObjectNode jsonObject) {
    try {
      JsonNode jsonNode = jsonObject.get(Const.SEGMENT);
      if (null != jsonNode) {
        String segmentStr = jsonNode.asText();
        SegmentDo segmentDo = JsonUtil.string2Obj(segmentStr, SegmentDo.class);
        segmentList.add(segmentDo);
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.getSegmentFromJSONObject() # 将来自skywalking探针的审计日志放入到 segmentList 表中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：insertSegment</B>
   * <B>概要说明：将来自skywalking的segment信息、索引信息和数据库操作SQL插入到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月30日 17:05:27
   * @Param [jsonObject]
   **/
  private void insertSegmentAndIndexAndAuditLog() {
    try {
      Instant now = Instant.now();
      long isShouldFlush = DateTimeUtil.getSecond(CURRENT_TIME) - flushToRocketMQInterval;
      if (isShouldFlush >= 0 || true == InitProcessorByLinkedBlockingQueue.getShutdown()) {
        // 当满足了间隔时间或者jvm进程退出时，就要把本地攒批的数据保存到MySQL数据库中；2022-06-01 10:38:04
        log.info("# IoThreadByEventHandler.insertSegmentAndIndexAndAuditLog() # 发送本地统计消息的时间间隔 = 【{}】秒.", flushToRocketMQInterval);

        mingshiServerUtil.flushUserNameToRedis(userHashSet);

        // 将processor线程发送到Redis中；2022-07-23 11:22:13
        mingshiServerUtil.flushProcessorThreadQpsToRedis(processorThreadQpsMap);

        // 将公共队列中有多少元素没有被消费发送到Redis中统计；2022-07-23 11:33:39
        mingshiServerUtil.flushIoThreadBatchInsertLinkedBlockingQueueSizeToRedis(ioThreadQueueSet);

        mingshiServerUtil.flushSegmentToDB(segmentList);
        // mingshiServerUtil.flushAuditLogToDB(auditLogList);

        // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
        mingshiServerUtil.flushSkywalkingAgentInformationToDb();

        // 将QPS信息刷入Redis中；2022-06-27 13:42:13
        // mingshiServerUtil.flushQpsToRedis();

        // 将Span信息刷入MySQL数据库中;
        mingshiServerUtil.flushSpansToDB(spanList);

        // 将探针名称发送到Redis中，用于心跳检测；2022-06-27 13:42:13
        mingshiServerUtil.flushSkywalkingAgentNameToRedis(skywalkingAgentHeartBeatMap);

        mingshiServerUtil.insertMonitorTables();

        // 不能再更新这个了，因为花的时间太久；2022-07-18 17:24:06
        // 比较好的做法是，把用户名、token、globalTraceId放到Redis中去存储，有一个定时任务，定时去MySQL中根据token和globalTraceId分组查询用户名为空的记录，然后拿着token和globalTraceId
        // 去Redis缓存中获取。如果获取到了用户名，那么就把用户名更新到MySQL数据库中。
        // mingshiServerUtil.updateUserNameByGlobalTraceId();

        mingshiServerUtil.flushSegmentDetailToDB(segmentDetailDoList);

        mingshiServerUtil.flushAbnormalToDB(msAlarmInformationDoLinkedListist);
        CURRENT_TIME = Instant.now();
      } else {
        // 减少log日志输出；2021-10-20 15:49:59
        if (Const.IOTREAD_LOG_INTERVAL <= DateTimeUtil.getSecond(now)) {
          log.info("# IoThreadByEventHandler.insertSegmentAndIndexAndAuditLog() # 当前IoThread统计线程离下次刷盘时间还有 = 【{}】秒。", isShouldFlush);
        }
      }
    } catch (Exception e) {
      log.error("# IoThreadByEventHandler.insertSegmentAndIndexAndAuditLog() # 将来自skywalking的segment信息和SQL审计信息插入到表中出现了异常。", e);
    }
  }

}
