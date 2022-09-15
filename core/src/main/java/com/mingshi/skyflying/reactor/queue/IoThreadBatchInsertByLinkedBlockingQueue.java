package com.mingshi.skyflying.reactor.queue;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.reactor.thread.IoThread;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import com.mingshi.skyflying.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
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

  /**
   * 阻塞队列中，元素的个数；2021-06-09 16:30:20
   */
  private final static Integer QUEUE_SIZE = Const.IO_THREAD_QUEUE_SIZE;
  private volatile static List<LinkedBlockingQueue<ObjectNode>> linkedBlockingQueueList = new ArrayList<>();
  /**
   * 单例的个数不能大于1，否则就不是单例了；2021-06-23 10:49:00
   */
  private volatile static AtomicInteger SINGLE_CASE_COUNT = new AtomicInteger(Const.NUMBER_ZERO);

  /**
   * 获取队列的容量；2021-11-17 14:35:19
   *
   * @return
   */
  public static Integer getQueueAllSize() {
    return QUEUE_SIZE;
  }

  public static List<LinkedBlockingQueue<ObjectNode>> getLinkedBlockingQueueList() {
    return linkedBlockingQueueList;
  }

  /**
   * 私有构造函数，只能产生一个单例；2021-06-23 10:49:23
   *
   * @param localStatisticsThreadCount
   * @param flushToRocketMqInterval
   * @param mingshiServerUtil
   */
  public IoThreadBatchInsertByLinkedBlockingQueue(boolean gracefulShutdown,Integer localStatisticsThreadCount, Integer flushToRocketMqInterval, MingshiServerUtil mingshiServerUtil) {
    log.info("开始执行方法OperatorRedisFailureBuffer（）。");
    if (0 < SINGLE_CASE_COUNT.get()) {
      log.error("# BatchInsertByLinkedBlockingQueue.BatchInsertByLinkedBlockingQueue() # 类OperatorRedisFailureBuffer的实例个数大于1了（【{}】），不允许再次创建实例。", SINGLE_CASE_COUNT);
      return;
    }
    SINGLE_CASE_COUNT.incrementAndGet();
    for (Integer integer = 0; integer < localStatisticsThreadCount; integer++) {
      LinkedBlockingQueue<ObjectNode> linkedBlockingQueue = new LinkedBlockingQueue(QUEUE_SIZE);
      linkedBlockingQueueList.add(linkedBlockingQueue);
      log.info("# BatchInsertByLinkedBlockingQueue.BatchInsertByLinkedBlockingQueue() # 开始创建第【{}】个IoThread线程。", (1 + integer));
      IoThread ioThread = new IoThread(gracefulShutdown, linkedBlockingQueue, flushToRocketMqInterval, mingshiServerUtil);
      ioThread.setName("processLocalStatisticsThread_" + integer);
      ioThread.start();
    }
  }

  /**
   * 获取单例；2021-06-23 10:50:06
   *
   * @param localStatisticsThreadCount
   * @param flushToRocketMqInterval
   * @param mingshiServerUtil
   * @return
   */
  public static LinkedBlockingQueue getLinkedBlockingQueue(boolean gracefulShutdown, Integer localStatisticsThreadCount, Integer flushToRocketMqInterval, MingshiServerUtil mingshiServerUtil, Integer partition) {
    if (null == linkedBlockingQueueList || 0 == linkedBlockingQueueList.size()) {
      synchronized (IoThreadBatchInsertByLinkedBlockingQueue.class) {
        if (null == linkedBlockingQueueList || 0 == linkedBlockingQueueList.size()) {
          log.info("获取单例LinkedBlockingQueue。");
          new IoThreadBatchInsertByLinkedBlockingQueue(gracefulShutdown, localStatisticsThreadCount, flushToRocketMqInterval, mingshiServerUtil);
        }
      }
    }

    Integer index = indexFor(partition, linkedBlockingQueueList.size());

    return linkedBlockingQueueList.get(index);
  }

  /**
   * <B>方法名称：getQueueIndex</B>
   * <B>概要说明：根据partition获取对应的队列</B>
   * @Author zm
   * @Date 2022年09月14日 19:09:54
   * @Param [partition]
   * @return java.lang.Integer
   **/
  public static Integer getQueueIndex(Integer partition){
    if(0 < linkedBlockingQueueList.size() && null != partition && -1 < partition){
      return indexFor(partition, linkedBlockingQueueList.size());
    }
    return -1000;
  }

  private static volatile Boolean TWO_POWER_FLAG = null;

  /**
   * <B>方法名称：indexFor</B>
   * <B>概要说明：采用按位与代替取模运算，从而提升性能</B>
   *
   * @return int
   * 注意：这里要确保只有一个地方能够调用这个方法。如果多个地方调用这个方法这里就不合适了，因为 变量 TWO_POWER_FLAG 只会被初始化一次。
   * @Author zm
   * @Date 2022年06月01日 09:06:57
   * @Param [h, length]
   **/
  private static int indexFor(int h, int length) {
    if (null == TWO_POWER_FLAG) {
      //
      if (true == ReactorUtil.isTwoPower(length)) {
        TWO_POWER_FLAG = true;
      } else {
        TWO_POWER_FLAG = false;
      }
    }
    if (!TWO_POWER_FLAG.equals(null) && true == TWO_POWER_FLAG) {
      // 使用按位与获取下标；2022-09-13 14:20:28
      return h & (length - 1);
    }
    // 当length不是2的幂次方的时候，使用取模获取下标。2022-09-13 14:21:07
    return h % length;
  }
}
