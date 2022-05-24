package com.mingshi.skyflying.reactor.thread;

import com.mingshi.skyflying.config.SingletonLocalStatisticsMap;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.UserTokenDao;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.domain.UserTokenDo;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.StringUtil;
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
  private LinkedBlockingQueue<SegmentDo> linkedBlockingQueue;
  private Instant CURRENT_TIME = Instant.now().minusSeconds(new Random().nextInt(30));
  private Integer flushToRocketMQInterval = 5;
  private LinkedList list = new LinkedList();
  private SegmentDao segmentDao;
  private UserTokenDao userTokenDao;

  public IoThread(LinkedBlockingQueue<SegmentDo> linkedBlockingQueue, Integer flushToRocketMQInterval, SegmentDao segmentDao, UserTokenDao userTokenDao) {
    // 防御性编程，当间隔为null或者小于0时，设置成5；2022-05-19 18:11:31
    if (null == flushToRocketMQInterval || flushToRocketMQInterval < 0) {
      this.flushToRocketMQInterval = flushToRocketMQInterval;
    }
    this.linkedBlockingQueue = linkedBlockingQueue;
    this.segmentDao = segmentDao;
    this.userTokenDao = userTokenDao;
  }

  // todo: 1. 前端查询时，不再根据 user_token 表关联 segment 表来获取数据，而是直接根据 segment 表来查询数据，因为 user_token 表里已经有了 用户名 、token、全局追踪id了。
  // user_token 表存在的意义是：将 segment 表中的全局追踪id与用户名或 token 关联起来。
  // 当前端查询 segment 表中的数据时，若是碰到了 用户名或者 token 为空，此时就去 user_token 表中根据全局追踪id查询对应的用户名或者 token。
  // todo: 2. 项目启动时，从 user_token 表中把用户名和 对应的 token 加载到内存中，方便后续的 url 操作匹配用户名和token。2022-05-24 18:37:48
  @Override
  public void run() {
    while (true) {
      try {
        SegmentDo segment = linkedBlockingQueue.poll();
        Instant now = Instant.now();
        if (null == segment) {
          TimeUnit.MILLISECONDS.sleep(10);
        } else {
          list.add(segment);
        }
        long isShouldFlush = DateTimeUtil.getSecond(CURRENT_TIME) - flushToRocketMQInterval;
        if (isShouldFlush >= 0) {
          log.info("发送本地统计消息的时间间隔 = 【{}】.", flushToRocketMQInterval);
          flushSegmentToDB(now);

          flushSegmentIndexToDB(now);

          CURRENT_TIME = Instant.now();
        } else {
          // 减少log日志输出；2021-10-20 15:49:59
          if (5 <= DateTimeUtil.getSecond(now)) {
            log.info("当前本地统计线程离下次刷盘时间还有 = 【{}】秒。", isShouldFlush);
          }
        }
      } catch (Throwable e) {
        log.error("将统计信息现在本地进行累加，然后再发送到Redis中。在这个过程中，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushToDB</B>
   * <B>概要说明：批量插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月19日 18:05:20
   * @Param []
   **/
  private void flushSegmentToDB(Instant now) {
    if (0 < list.size()) {
      try {
        segmentDao.insertSelectiveBatch(list);
        log.info("将【{}】条segment数据插入到表中，耗时【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
        list.clear();
      } catch (Exception e) {
        log.error("将segment数据批量插入到数据库中的时候，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSegmentIndexToDB</B>
   * <B>概要说明：将索引保存到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月24日 11:05:07
   * @Param [now]
   **/
  private void flushSegmentIndexToDB(Instant now) {
    Boolean atomicBoolean = SingletonLocalStatisticsMap.getAtomicBooleanIsChanged();
    if (false == atomicBoolean) {
      // 只有当索引有变动的时候，才把数据更新到数据库中；2022-05-24 17:15:55
      return;
    }

    Boolean booleanIsUpdatingData = SingletonLocalStatisticsMap.getAtomicBooleanIsUpdatingData();
    if (true == booleanIsUpdatingData) {
      // 只有其他线程没有执行刷新操作时，本线程才执行；2022-05-24 17:15:55
      return;
    }
    // 这里应该加个正在执行更新的标志；考虑这样一种场景：在同一个jvm进程内，有多个 IoThread 线程在执行，在同一时间应该只有一个 IoThread 线程执行更新操作。
    // 2022-05-24 17:17:55
    Map<String/* token */, String/* userName */> tokenUserNameMap = SingletonLocalStatisticsMap.getTokenAndUserNameMap();
    Map<String/* globalTraceId */, String/* userName */> globalTraceIdAndUserNameMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndUserNameMap();
    Map<String/* globalTraceId */, String/* token */> globalTraceIdTokenMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndTokenMapMap();
    Iterator<String> iterator = globalTraceIdAndUserNameMap.keySet().iterator();
    List<UserTokenDo> userTokenDoList = new LinkedList<>();
    while (iterator.hasNext()) {
      String globalTraceId = iterator.next();
      String userName = globalTraceIdAndUserNameMap.get(globalTraceId);
      String token = globalTraceIdTokenMap.get(globalTraceId);
      if (StringUtil.isBlank(userName)) {
        userName = tokenUserNameMap.get(token);
      }
      if (StringUtil.isBlank(userName)) {
        // log.error("# IoThread.flushSegmentIndexToDB # 将索引插入到数据库中的时候，出现了异常。userName = null，globalTraceId = 【{}】，token = 【{}】。", globalTraceId, token);
        continue;
      }

      UserTokenDo userTokenDo = new UserTokenDo();
      userTokenDo.setUserName(userName);
      userTokenDo.setGlobalTraceId(globalTraceId);
      userTokenDo.setToken(token);
      userTokenDoList.add(userTokenDo);
    }
    try {
      if (0 < userTokenDoList.size()) {
        userTokenDao.insertSelectiveBatch(userTokenDoList);
        log.info("当前线程【{}】将segment数据对应的索引（{}条）插入到表中，耗时【{}】毫秒。", Thread.currentThread().getName(), userTokenDoList.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("将索引存储到数据库中出现了异常。", e);
    }
    SingletonLocalStatisticsMap.setAtomicBooleanIsUpdatingData(false);
    SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(false);
  }
}
