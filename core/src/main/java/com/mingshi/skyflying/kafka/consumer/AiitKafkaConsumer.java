package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Optional;

@Component
@Slf4j
public class AiitKafkaConsumer {

  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Resource
  private SegmentConsumerService segmentConsumerService;

  @KafkaListener(topics = "skywalking-segments", groupId = "skyflying-consumer-group")
  public void onMessage(ConsumerRecord<String, Bytes> record, Acknowledgment ack) {
    Optional message = Optional.ofNullable(record.value());
    if (message.isPresent()) {
      if(true == reactorProcessorEnable){
        useReactorModel(record);
      }else{
        useNoReactorModel(record);
      }
      ack.acknowledge();
    }
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
  private void useNoReactorModel(ConsumerRecord<String, Bytes> record) {
    try {
      segmentConsumerService.consume(record, false);
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：useNoReactorModel</B>
   * <B>概要说明：使用reactor模式清洗调用链信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月19日 17:05:03
   * @Param [record]
   **/
  private void useReactorModel(ConsumerRecord<String, Bytes> record) {
    try {
      ReactorUtil.useReactorModelByLinkedBlockingQueue(record);
    } catch (Exception e) {
      log.error("将调用链信息放入到 processor线程对应的队列中的时候，出现了异常。", e);
    }
  }

}
