// package com.mingshi.skyflying.kafka.consumer;
//
// import lombok.extern.slf4j.Slf4j;
// import org.springframework.context.annotation.PropertySource;
// import org.springframework.stereotype.Component;
//
// import javax.annotation.Resource;
//
// @Component
// @Slf4j
// @PropertySource("classpath:application-${spring.profiles.active}.yml")
// public class AiitKafkaConsumer {
//   @Resource
//   private AiitKafkaConsumerUtil aiitKafkaConsumerUtil;
//
//   // @KafkaListener(topics = "skywalking-segments", groupId = "skyflying-consumer-group")
//   // public void onMessage(ConsumerRecord<String, Bytes> record, Acknowledgment ack) {
//   //   Optional message = Optional.ofNullable(record.value());
//   //   if (message.isPresent()) {
//   //     aiitKafkaConsumerUtil.doOnMessage(record);
//   //     ack.acknowledge();
//   //   }
//   // }
//
//   /**
//    * <B>方法名称：recordForwarding</B>
//    * <B>概要说明：将蓝景kafka服务器上的消息转发到我们内网测试环境中的kafka上</B>
//    *
//    * @return void
//    * @Author zm
//    * @Date 2022年06月24日 09:06:35
//    * @Param [record]
//    **/
//   // private void recordForwarding(ConsumerRecord<String, Bytes> record) {
//   //   try {
//   //     SegmentObject segmentObject = null;
//   //     segmentObject = SegmentObject.parseFrom(record.value().get());
//   //     aiitKafkaProducer.send(segmentObject);
//   //   } catch (InvalidProtocolBufferException e) {
//   //     e.printStackTrace();
//   //   }
//   // }
// }
