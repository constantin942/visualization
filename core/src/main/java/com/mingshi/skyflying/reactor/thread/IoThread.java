package com.mingshi.skyflying.reactor.thread;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.agent.AgentInformationSingleton;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.domain.SegmentDo;
import com.mingshi.skyflying.common.domain.Span;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.statistics.InformationOverviewSingleton;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * @Author zhaoming
 * @Description 每一个线程从队列中获取匹配后的数据，然后在本地统计；当超过了一定时间后，再统一发送到Redis中；
 * @Date 18:15 2021/10/13
 * @Param
 * @return
 **/
@Slf4j
public class IoThread extends Thread {
  // 所有的IoThread线程共享同一个公共有界阻塞队列；2022-06-01 10:22:49
  private LinkedBlockingQueue<ObjectNode> linkedBlockingQueue;
  private Instant currentTime = null;

  private Integer flushToRocketMqInterval = 10;
  private Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
  private Map<String/* 线程名称 */, Map<String/* 时间 */, Integer/* 消费的数量 */>> processorThreadQpsMap = null;
  private LinkedList<SegmentDo> segmentList = null;
  private LinkedList<MsSegmentDetailDo> segmentDetailDoList = null;
  private LinkedList<MsSegmentDetailDo> segmentDetailUserNameIsNullDoList = null;
  private HashSet<String> userHashSet = null;
  private LinkedList<Span> spanList = null;
  private List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist = null;
  private MingshiServerUtil mingshiServerUtil;

  public IoThread(LinkedBlockingQueue<ObjectNode> linkedBlockingQueue, Integer flushToRocketMqInterval, MingshiServerUtil mingshiServerUtil) {
    currentTime = Instant.now().minusSeconds(new Random().nextInt(30));
    // 懒汉模式：只有用到的时候，才创建list实例。2022-06-01 10:22:16
    skywalkingAgentHeartBeatMap = new HashMap<>(Const.NUMBER_EIGHT);
    processorThreadQpsMap = new HashMap<>(Const.NUMBER_EIGHT);
    segmentList = new LinkedList();
    userHashSet = new HashSet();
    segmentDetailDoList = new LinkedList();
    segmentDetailUserNameIsNullDoList = new LinkedList();
    spanList = new LinkedList();
    msAlarmInformationDoLinkedListist = new LinkedList();
    // 防御性编程，当间隔为null或者小于0时，设置成5；2022-05-19 18:11:31
    if (null == flushToRocketMqInterval || flushToRocketMqInterval < 0) {
      this.flushToRocketMqInterval = 5;
    }
    this.linkedBlockingQueue = linkedBlockingQueue;
    this.mingshiServerUtil = mingshiServerUtil;
  }

  @Override
  public void run() {
    try {
      while (true) {
        try {
          ObjectNode jsonObject = linkedBlockingQueue.poll();
          if (null == jsonObject) {
            TimeUnit.MILLISECONDS.sleep(10);
          } else {
            // 从json实例中获取探针名称信息，用于心跳；2022-06-27 13:40:44
            getSkywalkingAgentNameFromJsonObject(jsonObject);

            // 统计processorThread线程的QPS；2022-07-23 11:15:29
            getProcessorThreadQpsFromJsonObject(jsonObject);

            // getIoThreadQueueFromJSONObject(jsonObject);

            // 从json实例中获取segmentDetail实例的信息
            getSegmentDetailFromJsonObject(jsonObject);

            // 从json实例中获取用户名为空的segmentDetail实例信息
            getSegmentDetailUserNameIsNullFromJsonObject(jsonObject);

            // 从json实例中获取异常信息
            getAbnormalFromJsonObject(jsonObject);

            // 从json实例中获取segment的信息
            // getSegmentFromJSONObject(jsonObject);
          }

          // 将segment信息和SQL审计日志插入到表中；2022-05-30 17:50:12
          insertSegmentDetailIntoMySqlAndRedis();
        } catch (Throwable e) {
          log.error("# IoThread.run() # 将segment信息、及对应的索引信息和SQL审计日志信息在本地攒批和批量插入时 ，出现了异常。", e);
        }
      }
    } finally {
      // 当IoThread线程退出时，要把本地攒批的数据保存到MySQL数据库中；2022-06-01 10:32:43
      insertSegmentDetailIntoMySqlAndRedis();
      log.error("# IoThread.run() # IoThread线程要退出了。此时jvm关闭的标志位 = 【{}】，该线程对应的队列中元素的个数 = 【{}】。",
        InitProcessorByLinkedBlockingQueue.getShutdown(),
        linkedBlockingQueue.size());
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
  private void getAbnormalFromJsonObject(ObjectNode jsonObject) {
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
      log.error("# IoThread.getAbnormalFromJSONObject() # 将异常信息放入到 msAlarmInformationDoLinkedListist 中出现了异常。", e);
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
  private void getSkywalkingAgentNameFromJsonObject(ObjectNode jsonObject) {
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
      log.error("# IoThread.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getProcessorThreadQpsFromJSONObject</B>
   * <B>概要说明：获取QPS数据</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月23日 11:07:40
   * @Param [jsonObject]
   **/
  private void getProcessorThreadQpsFromJsonObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      JsonNode jsonNode = jsonObject.get(Const.QPS_ZSET_EVERY_PROCESSOR_THREAD);
      if (null != jsonNode) {
        listString = jsonNode.asText();
      }
      if (StringUtil.isNotBlank(listString)) {
        HashMap<String, Map<String, Integer>> map = JsonUtil.string2Obj(listString, HashMap.class);
        if (0 == processorThreadQpsMap.size()) {
          if (null != map && 0 < map.size()) {
            processorThreadQpsMap.putAll(map);
          }
        } else {
          Iterator<String> iterator = map.keySet().iterator();
          while (iterator.hasNext()) {
            String threadName = iterator.next();
            Map<String, Integer> timeCountMap = map.get(threadName);
            if (null == timeCountMap || 0 == timeCountMap.size()) {
              continue;
            }
            Map<String, Integer> map1 = processorThreadQpsMap.get(threadName);
            if (null == map1) {
              processorThreadQpsMap.put(threadName, timeCountMap);
              continue;
            }

            Iterator<String> iterator1 = timeCountMap.keySet().iterator();
            while (iterator1.hasNext()) {
              String time = iterator1.next();
              Integer count = timeCountMap.get(time);
              Integer integer = map1.get(time);
              if (null == integer) {
                map1.put(time, count);
              } else {
                map1.put(time, count + integer);
              }
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("# IoThread.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
    }
  }

  private void getSegmentDetailFromJsonObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      try {
        JsonNode jsonNode = jsonObject.get(Const.SEGMENT_DETAIL_DO_LIST);
        if (null != jsonNode) {
          listString = jsonNode.asText();
        }
      } catch (Exception e) {
        log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
      }
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<MsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, MsSegmentDetailDo.class);
        // 之所将用户名判断放到这里来判断，而不是放到processor线程中判断，原因是为了提高效率。
        // 如果要放到processor中进行判断，如果该用户不存在，那么需要将其保存到Redis中。
        // 在保存到Redis中的时候，需要将其加锁。一旦在processor中加锁，就降低了processor线程的性能。
        for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailList) {
          String userName = msSegmentDetailDo.getUserName();
          String tableName = msSegmentDetailDo.getMsTableName();
          if (StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(tableName)) {
            // 判断用户是否已存在，如果不存在，那么先暂存起来，然后再将其发送到Redis中；2022-07-19 10:03:22
            Boolean userIsExisted = InformationOverviewSingleton.userIsExisted(userName);
            if (false == userIsExisted) {
              userHashSet.add(userName);
            }
          }
        }
        segmentDetailDoList.addAll(segmentDetailList);
      }
    } catch (Exception e) {
      log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
    }
  }

  private void getSegmentDetailUserNameIsNullFromJsonObject(ObjectNode jsonObject) {
    try {
      String listString = null;
      try {
        JsonNode jsonNode = jsonObject.get(Const.SEGMENT_DETAIL_USERNAME_IS_NULL_DO_LIST);
        if (null != jsonNode) {
          listString = jsonNode.asText();
        }
      } catch (Exception e) {
        log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
      }
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<MsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, MsSegmentDetailDo.class);
        segmentDetailUserNameIsNullDoList.addAll(segmentDetailList);
      }
    } catch (Exception e) {
      log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getSegmentFromJSONObject</B>
   * <B>概要说明：从json实例中获取segment的信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 10:06:21
   * @Param [jsonObject]
   **/
  private void getSegmentFromJsonObject(ObjectNode jsonObject) {
    try {
      JsonNode jsonNode = jsonObject.get(Const.SEGMENT);
      if (null != jsonNode) {
        String segmentStr = jsonNode.asText();
        SegmentDo segmentDo = JsonUtil.string2Obj(segmentStr, SegmentDo.class);
        segmentList.add(segmentDo);
      }
    } catch (Exception e) {
      log.error("# IoThread.getSegmentFromJSONObject() # 将来自skywalking探针的审计日志放入到 segmentList 表中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：insertSegmentDetailIntoMySQLAndRedis</B>
   * <B>概要说明：将来自skywalking的segmentDetail信息保存到MySQL数据库和Redis分布式缓存中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月30日 17:05:27
   * @Param [jsonObject]
   **/
  private void insertSegmentDetailIntoMySqlAndRedis() {
    try {
      Instant now = Instant.now();
      long isShouldFlush = DateTimeUtil.getSecond(currentTime) - flushToRocketMqInterval;
      if (isShouldFlush >= 0 || true == InitProcessorByLinkedBlockingQueue.getShutdown()) {
        // 当满足了间隔时间或者jvm进程退出时，就要把本地攒批的数据保存到MySQL数据库中；2022-06-01 10:38:04
        mingshiServerUtil.doInsertSegmentDetailIntoMySqlAndRedis(userHashSet, processorThreadQpsMap, segmentList, spanList, skywalkingAgentHeartBeatMap, segmentDetailDoList, segmentDetailUserNameIsNullDoList, msAlarmInformationDoLinkedListist);
        currentTime = Instant.now();
        log.info("# IoThread.insertSegmentDetailIntoMySQLAndRedis() # 当前线程【{}】持久化一次数据操作，用时【{}】毫秒。", Thread.currentThread().getName(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# IoThread.insertSegmentAndIndexAndAuditLog() # 将来自skywalking的segment信息和SQL审计信息插入到表中出现了异常。", e);
    }
  }
}
