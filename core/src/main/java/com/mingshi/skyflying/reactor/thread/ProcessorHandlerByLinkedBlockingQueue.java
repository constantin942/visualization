package com.mingshi.skyflying.reactor.thread;

import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.time.Instant;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

@Slf4j
public class ProcessorHandlerByLinkedBlockingQueue implements Runnable {
  private LinkedBlockingQueue<ConsumerRecord<String, Bytes>> linkedBlockingQueue;
  private final Integer queueNoBatchSize = 256;
  private Instant now = Instant.now();
  private Integer queueSize = 0;
  private SegmentConsumerService segmentConsumerService;
  private AtomicInteger atomicInteger = new AtomicInteger(0);

  public ProcessorHandlerByLinkedBlockingQueue(SegmentConsumerService segmentConsumerService) {
    this.segmentConsumerService = segmentConsumerService;
    this.linkedBlockingQueue = new LinkedBlockingQueue<>(queueNoBatchSize);
    queueSize = queueNoBatchSize;
  }

  public boolean offer(ConsumerRecord<String, Bytes> record) {
    try {
      if (0 == atomicInteger.incrementAndGet() % 200) {
        // 每200条消息打印一次日志，否则会影响系统性能；2022-01-14 10:57:15
        log.info("将调用链信息放入到processor队列中，当前队列中的元素个数【{}】，队列的容量【{}】。", linkedBlockingQueue.size(), queueSize);
      }
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
            log.info("当前 processor 线程【{}】对应的队列为空【{}】，休眠50毫秒。再尝试从队列里获取数据。", Thread.currentThread().getName(), linkedBlockingQueue.size());
            now = Instant.now();
          }
          TimeUnit.MILLISECONDS.sleep(100);
        } else {
          // Instant startTime = Instant.now();
          // 处理业务逻辑；2022-05-19 17:47:18
          segmentConsumerService.consume(record, true);
        }
      } catch (Throwable e) {
        log.error("线程【{}】在清洗调用链信息时，出现了异常。", e);
      }
    }
  }
}
