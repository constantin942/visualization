package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.common.utils.RedisPoolUtil;
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
  @Resource
  private RedisPoolUtil redisPoolUtil;

  /**
   * <B>方法名称：useReactorModelByDisruptor</B>
   * <B>概要说明：使用Disruptor无锁高性能队列</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月22日 20:07:28
   * @Param [record]
   **/
  public void useReactorModelByDisruptor(ConsumerRecord<String, Bytes> consumerRecord) {
    try {
      while (Boolean.FALSE.equals(processorByDisruptor.getCreateProcessorsFinishedFlag())) {
        log.error("Disruptor还没有创建完毕，等待一会。");
        TimeUnit.SECONDS.sleep(50);
      }
    } catch (Exception e) {
      log.error("在等待创建Disruptor完毕时，出现了异常。", e);
    }
    processorByDisruptor.offer(consumerRecord);
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
  public void useNoReactorModel(ConsumerRecord<String, Bytes> consumerRecord) {
    try {
      segmentConsumerService.consume(consumerRecord, false);
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
  }

  public void doOnMessage(ConsumerRecord<String, Bytes> consumerRecord) {
    if (Boolean.TRUE.equals(reactorProcessorEnable)) {
      // 使用Reactor模式；
      if (Boolean.TRUE.equals(reactorProcessorByDisruptor)) {
        // 使用Disruptor无锁高性能队列；2022-07-22 20:57:02
        useReactorModelByDisruptor(consumerRecord);
      } else {
        // 使用LinkedBlockingQueue两把锁队列；2022-07-22 20:57:19
        ReactorUtil.useReactorModelByLinkedBlockingQueue(redisPoolUtil,consumerRecord);
      }
    } else {
      // 不使用Reactor模式；
      useNoReactorModel(consumerRecord);
    }
  }
}
