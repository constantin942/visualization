package com.mingshi.skyflying.reactor.thread;

import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.domain.SegmentDo;
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
  private LinkedBlockingQueue<JSONObject> linkedBlockingQueue;
  private Instant CURRENT_TIME = Instant.now().minusSeconds(new Random().nextInt(30));
  private Integer flushToRocketMQInterval = 1;
  private LinkedList<SegmentDo> segmentList = new LinkedList();
  private LinkedList<MsAuditLogDo> auditLogList = new LinkedList();
  private MingshiServerUtil mingshiServerUtil;

  public IoThread(LinkedBlockingQueue<JSONObject> linkedBlockingQueue, Integer flushToRocketMQInterval, SegmentDao segmentDao, MingshiServerUtil mingshiServerUtil) {
    // public IoThread(LinkedBlockingQueue<SegmentDo> linkedBlockingQueue, Integer flushToRocketMQInterval, SegmentDao segmentDao, UserTokenDao userTokenDao) {
    // 防御性编程，当间隔为null或者小于0时，设置成5；2022-05-19 18:11:31
    if (null == flushToRocketMQInterval || flushToRocketMQInterval < 0) {
      this.flushToRocketMQInterval = flushToRocketMQInterval;
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
    while (true) {
      try {
        Instant now = Instant.now();
        JSONObject jsonObject = linkedBlockingQueue.poll();
        if (null == jsonObject) {
          TimeUnit.MILLISECONDS.sleep(10);
          continue;
        }
        // 将来自skywalking探针的审计日志插入到表中；2022-05-30 17:48:31
        try {
          String listString = jsonObject.getString("auditLogFromSkywalkingAgentList");
          if(StringUtil.isNotBlank(listString)){
            LinkedList<MsAuditLogDo> auditLogFromSkywalkingAgentList = JsonUtil.string2Obj(listString, LinkedList.class, MsAuditLogDo.class);
            auditLogList.addAll(auditLogFromSkywalkingAgentList);
          }
        } catch (Exception e) {
          log.error("# IoThread.run() # 将来自skywalking探针的审计日志放入到 auditLogList 表中出现了异常。", e);
        }
        try {
          String segmentStr = jsonObject.getString("segment");
          SegmentDo segmentDo = JsonUtil.string2Obj(segmentStr, SegmentDo.class);
          // SegmentDo segment = linkedBlockingQueue.poll();
          if (null == segmentDo) {
            TimeUnit.MILLISECONDS.sleep(10);
          } else {
            segmentList.add(segmentDo);
          }
        } catch (Exception e) {
          log.error("# IoThread.run() # 将来自skywalking探针的审计日志放入到 segmentList 表中出现了异常。", e);
        }

        // 将segment信息插入到表中；2022-05-30 17:50:12
        insertSegmentAndIndexAndAuditLog(now);
      } catch (Throwable e) {
        log.error("# IoThread.run() # 将segment信息、及对应的索引信息和SQL审计日志信息在本地攒批和批量插入时 ，出现了异常。", e);
      }
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
  private void insertSegmentAndIndexAndAuditLog(Instant now) {
    try {
      long isShouldFlush = DateTimeUtil.getSecond(CURRENT_TIME) - flushToRocketMQInterval;
      if (isShouldFlush >= 0) {
        log.info("发送本地统计消息的时间间隔 = 【{}】.", flushToRocketMQInterval);
        mingshiServerUtil.flushSegmentToDB(segmentList);
        mingshiServerUtil.flushAuditLogToDB( auditLogList);
        mingshiServerUtil.flushSegmentIndexToDB();
        CURRENT_TIME = Instant.now();
      } else {
        // 减少log日志输出；2021-10-20 15:49:59
        if (5 <= DateTimeUtil.getSecond(now)) {
          log.info("当前本地统计线程离下次刷盘时间还有 = 【{}】秒。", isShouldFlush);
        }
      }
    } catch (Exception e) {
      log.error("# IoThread.insertSegment() # 将来自skywalking的segment信息插入到表中出现了异常。", e);
    }
  }
}
