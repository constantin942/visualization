package com.mingshi.skyflying.reactor.queue;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.utils.EsMsSegmentDetailUtil;
import com.mingshi.skyflying.reactor.thread.IoThread;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @ClassName OperatorRedisFailure
 * @Description @Author apple
 * Date 2021/6/9 下午4:25
 * @Version 1.0
 **/
@Slf4j
public class IoThreadBatchInsertByLinkedBlockingQueue {

  // 阻塞队列中，元素的个数；2021-06-09 16:30:20
  private final static Integer QUEUE_SIZE = 5000;
  // private final static Integer QUEUE_SIZE = 500;
  private volatile static LinkedBlockingQueue<ObjectNode> linkedBlockingQueue = null;
  // 单例的个数不能大于1，否则就不是单例了；2021-06-23 10:49:00
  private volatile static AtomicInteger SINGLE_CASE_COUNT = new AtomicInteger(0);

  // 获取队列的容量；2021-11-17 14:35:19
  public static Integer getQueueAllSize() {
    return QUEUE_SIZE;
  }

  // 获取队列中元素的个数；2022-07-26 17:27:02
  public static Integer getQueueSize() {
    return linkedBlockingQueue.size();
  }

  // 私有构造函数，只能产生一个单例；2021-06-23 10:49:23
  public IoThreadBatchInsertByLinkedBlockingQueue(Integer localStatisticsThreadCount, Integer flushToRocketMqInterval, MingshiServerUtil mingshiServerUtil, EsMsSegmentDetailUtil esMsSegmentDetailUtil) {
    log.info("开始执行方法OperatorRedisFailureBuffer（）。");
    if (0 < SINGLE_CASE_COUNT.get()) {
      log.error("# BatchInsertByLinkedBlockingQueue.BatchInsertByLinkedBlockingQueue() # 类OperatorRedisFailureBuffer的实例个数大于1了（【{}】），不允许再次创建实例。", SINGLE_CASE_COUNT);
      return;
    }
    SINGLE_CASE_COUNT.incrementAndGet();
    linkedBlockingQueue = new LinkedBlockingQueue(QUEUE_SIZE);
    for (Integer integer = 0; integer < localStatisticsThreadCount; integer++) {
      log.info("# BatchInsertByLinkedBlockingQueue.BatchInsertByLinkedBlockingQueue() # 开始创建第【{}】个IoThread线程。",(1 + integer));
      IoThread ioThread = new IoThread(linkedBlockingQueue, flushToRocketMqInterval, mingshiServerUtil, esMsSegmentDetailUtil);
      ioThread.setName("processLocalStatisticsThread_" + integer);
      ioThread.start();
    }
  }

  // 获取单例；2021-06-23 10:50:06
  public static LinkedBlockingQueue getLinkedBlockingQueue(Integer localStatisticsThreadCount, Integer flushToRocketMqInterval, MingshiServerUtil mingshiServerUtil, EsMsSegmentDetailUtil esMsSegmentDetailUtil) {
    if (null == linkedBlockingQueue) {
      synchronized (IoThreadBatchInsertByLinkedBlockingQueue.class) {
        if (null == linkedBlockingQueue) {
          log.info("获取单例LinkedBlockingQueue。");
          new IoThreadBatchInsertByLinkedBlockingQueue(localStatisticsThreadCount, flushToRocketMqInterval, mingshiServerUtil, esMsSegmentDetailUtil);
        }
      }
    }
    return linkedBlockingQueue;
  }
}
