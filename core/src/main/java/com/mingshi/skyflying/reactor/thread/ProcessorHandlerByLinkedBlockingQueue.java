package com.mingshi.skyflying.reactor.thread;

import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.time.Instant;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

@Slf4j
public class ProcessorHandlerByLinkedBlockingQueue implements Runnable {

  // 这里使用LinkedBlockingQueue的原因是：该阻塞队列有两把独占锁，分别是入队列的独占锁和出队列的独占锁。
  // 当线程执行入队列操作时，不影响操作出队列的线程。也就是说，执行入队列的线程与执行出队列的线程互不影响。2022-06-01 09:47:30
  private LinkedBlockingQueue<ConsumerRecord<String, Bytes>> linkedBlockingQueue;

  // 队列里存放的消息的个数；2022-06-01 09:42:19
  private final Integer queueNoBatchSize = 256;

  private Instant now = Instant.now();
  private SegmentConsumerService segmentConsumerService;
  private Integer count = 0;

  public ProcessorHandlerByLinkedBlockingQueue(SegmentConsumerService segmentConsumerService) {
    this.segmentConsumerService = segmentConsumerService;
    this.linkedBlockingQueue = new LinkedBlockingQueue<>(queueNoBatchSize);
  }

  public boolean offer(ConsumerRecord<String, Bytes> record) {
    try {
      if (0 == ++count % 200) {
        // 每200条消息打印一次日志，否则会影响系统性能；2022-01-14 10:57:15
        log.info("将调用链信息放入到processor队列中，当前队列中的元素个数【{}】，队列的容量【{}】。", linkedBlockingQueue.size(), queueNoBatchSize);
      }

      // 这里之所以使用阻塞队列的offer方法，是为了提升性能，提升性能的点：当队列满时，在不加锁的情况下，直接返回false。2022-06-01 09:44:53
      return linkedBlockingQueue.offer(record);
    } catch (Exception e) {
      log.error("将调用链信息(record)放入到LinkedBlockingQueue中出现了异常。", e);
      return false;
    }
  }

  // 获取队列中元素的个数；2021-10-20 15:22:55
  public Integer getQueueSize() {
    return linkedBlockingQueue.size();
  }

  @Override
  public void run() {
    // todo：这里最好设置一个标志位，当jvm关闭时，将标志位设置为false，那么线程就可以正常退出了；2022-06-01 09:40:44
    while (!Thread.interrupted()) {
      try {
        ConsumerRecord<String, Bytes> record = linkedBlockingQueue.poll();
        if (null == record) {
          if (10 <= DateTimeUtil.getSecond(now)) {
            // 提升性能：当队列为空的时候，每10秒打印一次日志。2022-06-01 09:50:19
            log.info("当前 processor 线程【{}】对应的队列为空，休眠50毫秒。再尝试从队列里获取数据。", Thread.currentThread().getName());
            now = Instant.now();
          }
          TimeUnit.MILLISECONDS.sleep(50);
        } else {
          segmentConsumerService.consume(record, true);
        }
      } catch (Throwable e) {
        log.error("线程【{}】在清洗调用链信息时，出现了异常。", e);
      }
    }
  }
}
