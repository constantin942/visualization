package com.mingshi.skyflying.kafka.consumer;

import com.google.protobuf.InvalidProtocolBufferException;
import com.mingshi.skyflying.disruptor.ProcessorByDisruptor;
import com.mingshi.skyflying.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.support.Acknowledgment;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class AiitKafkaConsumer {

  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorByDisruptor;

  @Resource
  private ProcessorByDisruptor processorByDisruptor;
  @Resource
  private SegmentConsumerService segmentConsumerService;
  @Resource
  private AiitKafkaProducer aiitKafkaProducer;

  @KafkaListener(topics = "skywalking-segments", groupId = "skyflying-consumer-group")
  public void onMessage(ConsumerRecord<String, Bytes> record, Acknowledgment ack) {
    Optional message = Optional.ofNullable(record.value());
    if (message.isPresent()) {
      if(true == reactorProcessorEnable){
        // 使用Reactor模式；
        if(true == reactorProcessorByDisruptor){
          // 使用Disruptor无锁高性能队列；2022-07-22 20:57:02
          useReactorModelByDisruptor(record);
        }else{
          // 使用LinkedBlockingQueue两把锁队列；2022-07-22 20:57:19
          useReactorModelByLinkedBlockingQueue(record);
        }
      }else{
        // 不使用Reactor模式；
        useNoReactorModel(record);
      }
      ack.acknowledge();
    }
  }

  /**
   * <B>方法名称：recordForwarding</B>
   * <B>概要说明：将蓝景kafka服务器上的消息转发到我们内网测试环境中的kafka上</B>
   * @Author zm
   * @Date 2022年06月24日 09:06:35
   * @Param [record]
   * @return void
   **/
  private void recordForwarding(ConsumerRecord<String, Bytes> record) {
    try {
      SegmentObject segmentObject = null;
      segmentObject = SegmentObject.parseFrom(record.value().get());
      aiitKafkaProducer.send(segmentObject);
    } catch (InvalidProtocolBufferException e) {
      e.printStackTrace();
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
  private void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> record) {
    ReactorUtil.useReactorModelByLinkedBlockingQueue(record);
  }

  /**
   * <B>方法名称：useReactorModelByDisruptor</B>
   * <B>概要说明：使用Disruptor无锁高性能队列</B>
   * @Author zm
   * @Date 2022年07月22日 20:07:28
   * @Param [record]
   * @return void
   **/
  private void useReactorModelByDisruptor(ConsumerRecord<String, Bytes> record) {
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

}
