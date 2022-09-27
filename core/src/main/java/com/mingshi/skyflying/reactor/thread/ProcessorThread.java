package com.mingshi.skyflying.reactor.thread;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.time.Instant;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

@Slf4j
public class ProcessorThread extends Thread {

  /**
   * 这里使用LinkedBlockingQueue的原因是：该阻塞队列有两把独占锁，分别是入队列的独占锁和出队列的独占锁。
   * 当线程执行入队列操作时，不影响操作出队列的线程。也就是说，执行入队列的线程与执行出队列的线程互不影响。2022-06-01 09:47:30
   */
  private LinkedBlockingQueue<ConsumerRecord<String, Bytes>> linkedBlockingQueue;

  /**
   * 队列里存放的消息的个数；2022-06-01 09:42:19
   */
  private final Integer queueSize = Const.QUEUE_SIZE;

  private Instant now = Instant.now();
  private SegmentConsumerService segmentConsumerService;
  private Integer count = 0;

  public ProcessorThread(SegmentConsumerService segmentConsumerService) {
    this.segmentConsumerService = segmentConsumerService;
    this.linkedBlockingQueue = new LinkedBlockingQueue<>(queueSize);
  }

  /**
   * <B>方法名称：offer</B>
   * <B>概要说明：往Processor线程自己的队列中放入数据</B>
   * @Author zm
   * @Date 2022年09月27日 14:09:23
   * @Param [consumerRecord]
   * @return boolean
   **/
  public boolean offer(ConsumerRecord<String, Bytes> consumerRecord) {
    try {
      if (++count > (Const.RECORD_COUNT)) {
        // 每10万条消息打印一次日志，否则会影响系统性能；2022-01-14 10:57:15
        log.info("将调用链信息放入到processor队列中，当前队列中的元素个数【{}】，队列的容量【{}】。", linkedBlockingQueue.size(), queueSize);
        count = 0;
      }

      // 这里之所以使用阻塞队列的offer方法，是为了提升性能，提升性能的点：当队列满时，在不加锁的情况下，直接返回false。2022-06-01 09:44:53
      return linkedBlockingQueue.offer(consumerRecord);
    } catch (Exception e) {
      log.error("将调用链信息(record)放入到LinkedBlockingQueue中出现了异常。", e);
      return false;
    }
  }

  /**
   * 获取队列中元素的个数；2021-10-20 15:22:55
   * @return
   */
  public Integer getQueueSize() {
    return linkedBlockingQueue.size();
  }

  @Override
  public void run() {
    while (false == InitProcessorByLinkedBlockingQueue.getShutdown()) {
      try {
        ConsumerRecord<String, Bytes> consumerRecord = linkedBlockingQueue.poll();
        if (null == consumerRecord) {
          if (Const.NUMBER_TEN <= DateTimeUtil.getSecond(now)) {
            // 提升性能：当队列为空的时候，每10秒打印一次日志。2022-06-01 09:50:19
            log.info("当前 processor 线程【{}】对应的队列为空，休眠50毫秒。再尝试从队列里获取数据。", Thread.currentThread().getName());
            now = Instant.now();
          }
          TimeUnit.MILLISECONDS.sleep(Const.SLEEP_INTERVAL);
        } else {
          segmentConsumerService.consume(consumerRecord, true);
        }
      } catch (Throwable e) {
        log.error("线程【{}】在清洗调用链信息时，出现了异常。", e);
      }
    }
    log.error("# ProcessorHandlerByLinkedBlockingQueue.run() # processor线程要退出了。此时jvm关闭的标志位 = 【{}】，该线程对应的队列中元素的个数 = 【{}】。",
      InitProcessorByLinkedBlockingQueue.getShutdown(),
      getQueueSize());
  }
}
