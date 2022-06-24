package com.mingshi.skyflying.kafka.producer;

import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Component;
import org.springframework.util.concurrent.ListenableFuture;
import org.springframework.util.concurrent.ListenableFutureCallback;

import javax.annotation.Resource;

@Component
@Slf4j
public class AiitKafkaProducer {

  @Resource
  private KafkaTemplate<String, Object> kafkaTemplate;

  //自定义topic
  public static final String TOPIC_TEST = "skywalking-segments";

  public void send(String topic, String obj) {
    //发送消息
    ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(topic, obj);
    future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
      @Override
      public void onFailure(Throwable throwable) {
        //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
        log.info("发送消息失败  *** 发送消息失败 *** 发送消息失败的异步回调，topic = 【{}】，msg = 【{}】", topic, throwable.getMessage());
      }

      @Override
      public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
        //成功的处理
        log.info("发送消息成功的异步回调，topic = 【{}】，msg = 【{}】", topic, stringObjectSendResult.toString());
      }
    });
  }

  public void send(SegmentObject segmentObject) {
    //发送消息
    ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(TOPIC_TEST, Bytes.wrap(segmentObject.toByteArray()));
    future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
      @Override
      public void onFailure(Throwable throwable) {
        //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
        log.info("发送消息失败  *** 发送消息失败 *** 发送消息失败的异步回调，topic = 【{}】，msg = 【{}】", TOPIC_TEST, throwable.getMessage());
      }

      @Override
      public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
        //成功的处理
        // log.info("发送消息成功的异步回调，topic = 【{}】，msg = 【{}】", TOPIC_TEST, stringObjectSendResult.toString());
      }
    });
  }
}
