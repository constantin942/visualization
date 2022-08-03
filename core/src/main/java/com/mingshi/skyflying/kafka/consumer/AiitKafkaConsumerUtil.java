package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.disruptor.processor.ProcessorByDisruptor;
import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: AiitKafkaConsumerUtil</B>
 * <B>概要说明：</B>
 * @Author zm
 * Date 2022/7/28 17:02
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class AiitKafkaConsumerUtil {

  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorByDisruptor;

  @Resource
  private ProcessorByDisruptor processorByDisruptor;
  @Resource
  private SegmentConsumerService segmentConsumerService;

  // private volatile Integer count = 0;
  // private Instant now = Instant.now();
  // private static Map<String/* 线程名称 */, Map<String/* 时间 */, AtomicInteger/* 消息数量 */>> map = new ConcurrentHashMap<>();

  /**
   * <B>方法名称：useReactorModelByDisruptor</B>
   * <B>概要说明：使用Disruptor无锁高性能队列</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月22日 20:07:28
   * @Param [record]
   **/
  public void useReactorModelByDisruptor(ConsumerRecord<String, Bytes> record) {
    try {
      while (false == processorByDisruptor.getCreateProcessorsFinishedFlag()) {
        log.error("Disruptor还没有创建完毕，等待一会。");
        TimeUnit.SECONDS.sleep(50);
      }
    } catch (Exception e) {
      log.error("在等待创建Disruptor完毕时，出现了异常。", e);
    }
    processorByDisruptor.offer(record.value().get());
  }

  /**
   * <B>方法名称：useNoReactorModel</B>
   * <B>概要说明：使用非reactor模式清洗调用链信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月19日 17:05:03
   * @Param [record]
   **/
  public void useNoReactorModel(ConsumerRecord<String, Bytes> record) {
    try {
      segmentConsumerService.consume(record, false);
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
  }

  public void doOnMessage(ConsumerRecord<String, Bytes> record) {
    // 统计每秒钟kafka消费者能够拿到多少消息
    // statisticsRecordCount();
    if (true == reactorProcessorEnable) {
      // 使用Reactor模式；
      if (true == reactorProcessorByDisruptor) {
        // 使用Disruptor无锁高性能队列；2022-07-22 20:57:02
        useReactorModelByDisruptor(record);
        // printLog(true);
      } else {
        // 使用LinkedBlockingQueue两把锁队列；2022-07-22 20:57:19
        ReactorUtil.useReactorModelByLinkedBlockingQueue(record);
        // printLog(false);
      }
    } else {
      // 不使用Reactor模式；
      useNoReactorModel(record);
    }
  }


  // public static Map<String, Map<String, AtomicInteger>> getMap() {
  //   return map;
  // }

  /**
   * <B>方法名称：statisticsRecordCount</B>
   * <B>概要说明：统计每秒钟kafka消费者能够拿到多少消息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月28日 13:07:24
   * @Param []
   **/
  // private void statisticsRecordCount() {
  //   String currentTime = DateTimeUtil.dateToStr(new Date());
  //   String threadName = Thread.currentThread().getName();
  //   Map<String, AtomicInteger> stringAtomicIntegerMap = map.get(threadName);
  //   if (null == stringAtomicIntegerMap) {
  //     ConcurrentHashMap<String, AtomicInteger> timeCountMap = new ConcurrentHashMap<>();
  //     map.put(threadName, timeCountMap);
  //     timeCountMap.put(currentTime, new AtomicInteger(1));
  //   } else {
  //     AtomicInteger count = stringAtomicIntegerMap.get(currentTime);
  //     if (null == count) {
  //       stringAtomicIntegerMap.put(currentTime, new AtomicInteger(1));
  //     } else {
  //       count.incrementAndGet();
  //       stringAtomicIntegerMap.put(currentTime, count);
  //     }
  //   }
  // }

  // private void printLog(Boolean isDisruptor) {
  //   if (++count >= 10 * 10000) {
  //     if (true == isDisruptor) {
  //       log.info(" # AiitKafkaConsumer.onMessage() # 将【{}】条消息放入到Disruptor队列中，耗时【{}】毫秒。", count, DateTimeUtil.getTimeMillis(now));
  //     } else {
  //       log.info(" # AiitKafkaConsumer.onMessage() # 将【{}】条消息放入到LinkedBlockingQueue队列中，耗时【{}】毫秒。", count, DateTimeUtil.getTimeMillis(now));
  //     }
  //     now = Instant.now();
  //     count = 0;
  //   }
  // }

}
