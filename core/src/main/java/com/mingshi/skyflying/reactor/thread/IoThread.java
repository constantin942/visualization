package com.mingshi.skyflying.reactor.thread;

import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.util.LinkedList;
import java.util.Random;
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
  private LinkedBlockingQueue<JSONObject> linkedBlockingQueue;
  private Instant CURRENT_TIME = null;
  private Integer flushToRocketMQInterval = 3;
  private LinkedList<SegmentDo> segmentList = null;
  private LinkedList<MsAuditLogDo> auditLogList = null;
  private LinkedList<MsSegmentDetailDo> segmentDetailDoList = null;
  private MingshiServerUtil mingshiServerUtil;

  public IoThread(LinkedBlockingQueue<JSONObject> linkedBlockingQueue, Integer flushToRocketMQInterval, MingshiServerUtil mingshiServerUtil) {
    CURRENT_TIME = Instant.now().minusSeconds(new Random().nextInt(30));
    // 懒汉模式：只有用到的时候，才创建list实例。2022-06-01 10:22:16
    segmentList = new LinkedList();
    auditLogList = new LinkedList();
    segmentDetailDoList = new LinkedList();
    // 防御性编程，当间隔为null或者小于0时，设置成5；2022-05-19 18:11:31
    if (null == flushToRocketMQInterval || flushToRocketMQInterval < 0) {
      this.flushToRocketMQInterval = 5;
    }
    this.linkedBlockingQueue = linkedBlockingQueue;
    this.mingshiServerUtil = mingshiServerUtil;
  }

  // todo: 1. 前端查询时，不再根据 user_token 表关联 segment 表来获取数据，而是直接根据 segment 表来查询数据，因为 user_token 表里已经有了 用户名 、token、全局追踪id了。
  // user_token 表存在的意义是：将 segment 表中的全局追踪id与用户名或 token 关联起来。
  // 当前端查询 segment 表中的数据时，若是碰到了 用户名或者 token 为空，此时就去 user_token 表中根据全局追踪id查询对应的用户名或者 token。
  // todo: 2. 项目启动时，从 user_token 表中把用户名和 对应的 token 加载到内存中，方便后续的 url 操作匹配用户名和token。2022-05-24 18:37:48
  @Override
  public void run() {
    try {
      while (false == InitProcessorByLinkedBlockingQueue.getShutdown()) {
        try {
          JSONObject jsonObject = linkedBlockingQueue.poll();
          if (null == jsonObject) {
            TimeUnit.MILLISECONDS.sleep(10);
          } else {
            // 从json实例中segmentDetail实例的信息
            getSegmentDetailFromJSONObject(jsonObject);

            // 从json实例中获取审计日志的信息
            // getAuditLogFromJSONObject(jsonObject);

            // 从json实例中获取segment的信息
            getSegmentFromJSONObject(jsonObject);
          }

          // 将segment信息和SQL审计日志插入到表中；2022-05-30 17:50:12
          insertSegmentAndIndexAndAuditLog();
        } catch (Throwable e) {
          log.error("# IoThread.run() # 将segment信息、及对应的索引信息和SQL审计日志信息在本地攒批和批量插入时 ，出现了异常。", e);
        }
      }
    } finally {
      // 当IoThread线程退出时，要把本地攒批的数据保存到MySQL数据库中；2022-06-01 10:32:43
      insertSegmentAndIndexAndAuditLog();
      log.error("# IoThread.run() # IoThread线程要退出了。此时jvm关闭的标志位 = 【{}】，该线程对应的队列中元素的个数 = 【{}】。",
        InitProcessorByLinkedBlockingQueue.getShutdown(),
        linkedBlockingQueue.size());
    }
  }

  /**
   * <B>方法名称：getSegmentDetailFromJSONObject</B>
   * <B>概要说明：将segmentDetail实例信息放入到 segmentDetailList 中</B>
   * @Author zm
   * @Date 2022年06月02日 11:06:40
   * @Param [jsonObject]
   * @return void
   **/
  private void getSegmentDetailFromJSONObject(JSONObject jsonObject) {
    try {
      String listString = jsonObject.getString(Const.SEGMENT_DETAIL_DO_LIST);
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<MsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, MsSegmentDetailDo.class);
        segmentDetailDoList.addAll(segmentDetailList);
      }
    } catch (Exception e) {
      log.error("# IoThread.run() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
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
  private void getAuditLogFromJSONObject(JSONObject jsonObject) {
    try {
      String listString = jsonObject.getString(Const.AUDITLOG_FROM_SKYWALKING_AGENT_LIST);
      if (StringUtil.isNotBlank(listString)) {
        LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgentList = JsonUtil.string2Obj(listString, LinkedList.class, MsAuditLogDo.class);
        auditLogList.addAll(auditLogFromSkywalkingAgentList);
      }
    } catch (Exception e) {
      log.error("# IoThread.run() # 将来自skywalking探针的审计日志放入到 auditLogList 表中出现了异常。", e);
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
  private void getSegmentFromJSONObject(JSONObject jsonObject) {
    try {
      String segmentStr = jsonObject.getString(Const.SEGMENT);
      SegmentDo segmentDo = JsonUtil.string2Obj(segmentStr, SegmentDo.class);
      if (null == segmentDo) {
        TimeUnit.MILLISECONDS.sleep(10);
      } else {
        segmentList.add(segmentDo);
      }
    } catch (Exception e) {
      log.error("# IoThread.run() # 将来自skywalking探针的审计日志放入到 segmentList 表中出现了异常。", e);
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
        log.info("# IoThread.insertSegmentAndIndexAndAuditLog() # 发送本地统计消息的时间间隔 = 【{}】.", flushToRocketMQInterval);
        // mingshiServerUtil.flushSegmentToDB(segmentList);
        // mingshiServerUtil.flushAuditLogToDB(auditLogList);
        mingshiServerUtil.flushSegmentIndexToDB();
        mingshiServerUtil.flushSegmentDetailToDB(segmentDetailDoList);
        CURRENT_TIME = Instant.now();
      } else {
        // 减少log日志输出；2021-10-20 15:49:59
        if (Const.IOTREAD_LOG_INTERVAL <= DateTimeUtil.getSecond(now)) {
          log.info("# IoThread.insertSegmentAndIndexAndAuditLog() # 当前IoThread统计线程离下次刷盘时间还有 = 【{}】秒。", isShouldFlush);
        }
      }
    } catch (Exception e) {
      log.error("# IoThread.insertSegmentAndIndexAndAuditLog() # 将来自skywalking的segment信息和SQL审计信息插入到表中出现了异常。", e);
    }
  }
}
