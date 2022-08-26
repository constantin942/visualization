// package com.mingshi.skyflying.kafka.consumer;
//
// import com.mingshi.skyflying.test.TestProcesserAndIoThreaderList;
// import com.mingshi.skyflying.test.domain.TestRecordAckDo;
// import com.mingshi.skyflying.test.thread.TestProcessorThread;
// import lombok.extern.slf4j.Slf4j;
// import org.apache.kafka.clients.consumer.ConsumerRecord;
// import org.apache.kafka.common.utils.Bytes;
// import org.springframework.context.annotation.PropertySource;
// import org.springframework.kafka.annotation.KafkaListener;
// import org.springframework.kafka.support.Acknowledgment;
// import org.springframework.stereotype.Component;
//
// import javax.annotation.Resource;
// import java.util.*;
//
// @Component
// @Slf4j
// @PropertySource("classpath:application-${spring.profiles.active}.yml")
// public class AiitKafkaConsumer {
//   @Resource
//   private AiitKafkaConsumerUtil aiitKafkaConsumerUtil;
//
//   private List<String> list = new ArrayList<>();
//
//   @KafkaListener(topics = "zm-test-topic-01", groupId = "zm-test-consumer-group")
//   public void onMessage(ConsumerRecord<String, Bytes> record, Acknowledgment ack) {
//     Optional message = Optional.ofNullable(record.value());
//     if (message.isPresent()) {
//       TestRecordAckDo testRecordAckDo = new TestRecordAckDo();
//       testRecordAckDo.setRecord(record);
//       testRecordAckDo.setAck(ack);
//
//       Integer partition = record.partition();
//       TestProcessorThread testProcessorThread = TestProcesserAndIoThreaderList.getTestProcessorThread(partition);
//       testProcessorThread.offer(testRecordAckDo);
//
//       long offset = record.offset();
//
//       String key = record.topic() + "#" + partition + "#" + offset;
//       if (list.contains(key)) {
//         log.error("当前线程 = {} 已包含 当前的offset = {}。", Thread.currentThread().getName(), key);
//       }else{
//         list.add(key);
//       }
//
//       // String topic = record.topic();
//       // Bytes value = record.value();
//       // Map<Integer, List<Long>> integerLongMap = map.get(topic);
//       // if(null == integerLongMap){
//       //   integerLongMap = new HashMap<>();
//       //   map.put(topic,integerLongMap);
//       // }
//       // List<Long> offsetList = integerLongMap.get(partition);
//       // if(null == offsetList){
//       //   offsetList = new LinkedList<>();
//       //   integerLongMap.put(partition,offsetList);
//       // }
//       // if(offsetList.contains(offset)){
//       //   log.error("");
//       // }else{
//       //   offsetList.add(offset);
//       // }
//     }
//   }
//   //
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
