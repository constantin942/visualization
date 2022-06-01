package com.mingshi.skyflying.reactor.queue;

import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.reactor.thread.IoThread;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @ClassName OperatorRedisFailure
 * @Description Author apple
 * Date 2021/6/9 下午4:25
 * @Version 1.0
 **/
@Slf4j
public class BatchInsertByLinkedBlockingQueue {

  // 阻塞队列中，元素的个数；2021-06-09 16:30:20
  private final static Integer QUEUE_SIZE = 500;
  // private volatile static LinkedBlockingQueue<SegmentDo> linkedBlockingQueue = null;
  private volatile static LinkedBlockingQueue<JSONObject> linkedBlockingQueue = null;
  // 单例的个数不能大于1，否则就不是单例了；2021-06-23 10:49:00
  private volatile static AtomicInteger SINGLE_CASE_COUNT = new AtomicInteger(0);

  // 获取队列的大小；2021-11-17 14:35:19
  public static Integer getQueueSize() {
    return QUEUE_SIZE;
  }

  // 私有构造函数，只能产生一个单例；2021-06-23 10:49:23
  public BatchInsertByLinkedBlockingQueue(Integer localStatisticsThreadCount, Integer flushToRocketMQInterval, SegmentDao segmentDao, MingshiServerUtil mingshiServerUtil) {
    log.info("开始执行方法OperatorRedisFailureBuffer（）。");
    if (0 < SINGLE_CASE_COUNT.get()) {
      log.error("类OperatorRedisFailureBuffer的实例个数大于1了（【{}】），不允许再次创建实例。", SINGLE_CASE_COUNT);
      return;
    }
    SINGLE_CASE_COUNT.incrementAndGet();
    linkedBlockingQueue = new LinkedBlockingQueue(QUEUE_SIZE);
    for (Integer integer = 0; integer < localStatisticsThreadCount; integer++) {
      IoThread ioThread = new IoThread(linkedBlockingQueue, flushToRocketMQInterval, segmentDao, mingshiServerUtil);
      ioThread.setName("processLocalStatisticsThread_" + integer);
      ioThread.start();
    }
  }

  // 获取单例；2021-06-23 10:50:06
  public static LinkedBlockingQueue getLinkedBlockingQueue(Integer localStatisticsThreadCount, Integer flushToRocketMQInterval, SegmentDao segmentDao, MingshiServerUtil mingshiServerUtil) {
    if (null == linkedBlockingQueue) {
      synchronized (BatchInsertByLinkedBlockingQueue.class) {
        if (null == linkedBlockingQueue) {
          log.info("获取单例LinkedBlockingQueue。");
          new BatchInsertByLinkedBlockingQueue(localStatisticsThreadCount, flushToRocketMQInterval, segmentDao, mingshiServerUtil);
        }
      }
    }
    return linkedBlockingQueue;
  }

}
