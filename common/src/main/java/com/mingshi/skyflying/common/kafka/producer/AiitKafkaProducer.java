package com.mingshi.skyflying.common.kafka.producer;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.PartitionInfo;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Component;
import org.springframework.util.concurrent.ListenableFuture;
import org.springframework.util.concurrent.ListenableFutureCallback;

import javax.annotation.Resource;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

@SuppressWarnings("ALL")
@Component
@Slf4j
public class AiitKafkaProducer {

    @Resource
    private KafkaTemplate<String, Object> kafkaTemplate;

    @Value("${spring.kafka.producer.topic}")
    private String producerTopic;
    private AtomicInteger PRINT_LOG_INTEVAL = new AtomicInteger(0);

    /**
     * 自定义topic
     */
    public Boolean sendWithSpecifyPartion(String topic, ObjectNode jsonNodes) {
        //发送消息
        try {
            String serviceInstance = jsonNodes.get("serviceInstance").asText();
            int length = serviceInstance.length();
            // 根据topic获取所有的partition信息；
            List<PartitionInfo> partitionInfos = kafkaTemplate.partitionsFor(topic);
            // 获取指定的partition；2022-08-25 10:57:15
            Integer partiton = length % partitionInfos.size();
            // 根据指定的partition，将消息发送出去；2022-08-25 10:57:31
            // 这里之所以指定partition，是因为需要将同一个探针的消息发送到同一个partition中。
            // 考虑这样一个场景：在同一时刻，如果对同一个探针执行多次开关操作，为了顺序的执行这些开关操作，这里将同一个探针的开关信息发送到一个partition中。
            //                探针那边是单线程在消费消息，这样一来，探针那边就可以顺序的执行开关操作了。如果将消息发送到多个partition中，会出现乱序的情况。
            ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(topic, partiton, serviceInstance, Bytes.wrap(jsonNodes.toString().getBytes()));
            future.get();
        } catch (Exception e) {
            log.error("# AiitKafkaProducer.send() # 发送消息到topic【{}】出现了异常。", topic, e);
            return false;
        }
        return true;
    }

    /**
     * <B>方法名称：sendWithKey</B>
     * <B>概要说明：将消息指定key发送到Kafka中</B>
     *
     * @Author zm
     * @Date 2022-10-19 09:50:55
     * @Param [topic, key, obj]
     * @return void
     **/
    public void sendWithKey(String topic, String key, String obj) {
        //发送消息
        ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(topic, key, Bytes.wrap(obj.getBytes()));
        future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
            @Override
            public void onFailure(Throwable throwable) {
                //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
                log.info(Const.SEND_FAIL, topic, throwable.getMessage());
            }

            @Override
            public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
                //成功的处理
                // ignore
            }
        });
    }

    /**
     * 自定义topic
     */
    public void send(String topic, String obj) {
        //发送消息
        ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(topic, Bytes.wrap(obj.getBytes()));
        future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
            @Override
            public void onFailure(Throwable throwable) {
                //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
                log.info(Const.SEND_FAIL, topic, throwable.getMessage());
            }

            @Override
            public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
                //成功的处理
                // ignore
            }
        });
    }

    public void send(MsSegmentDetailDo msSegmentDetailDo, String topic) {
        //发送消息
        ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(topic, Bytes.wrap(JsonUtil.obj2String(msSegmentDetailDo).getBytes()));
        future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
            @Override
            public void onFailure(Throwable throwable) {
                //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
                log.info("发送消息失败  *** 发送消息失败 *** 发送消息失败的异步回调，topic = 【{}】，msg = 【{}】", topic, throwable.getMessage());
            }

            @Override
            public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
                //成功的处理
            }
        });
    }

    public void send(SegmentObject segmentObject) {
        //发送消息
        ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(producerTopic, Bytes.wrap(segmentObject.toByteArray()));
        future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
            @Override
            public void onFailure(Throwable throwable) {
                //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
                log.info("发送消息失败  *** 发送消息失败 *** 发送消息失败的异步回调，topic = 【{}】，msg = 【{}】", producerTopic, throwable.getMessage());
            }

            @Override
            public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
                //成功的处理
            }
        });
    }

    public void send(SegmentObject segmentObject, String topic) {
        //发送消息
        ListenableFuture<SendResult<String, Object>> future = kafkaTemplate.send(topic, Bytes.wrap(segmentObject.toByteArray()));
        future.addCallback(new ListenableFutureCallback<SendResult<String, Object>>() {
            @Override
            public void onFailure(Throwable throwable) {
                //处理发送失败的情况；在这里做降级逻辑，将发送失败的消息要么存入到数据库中，要么写入本地磁盘中；
                log.info("发送消息失败  *** 发送消息失败 *** 发送消息失败的异步回调，topic = 【{}】，msg = 【{}】", topic, throwable.getMessage());
            }

            @Override
            public void onSuccess(SendResult<String, Object> stringObjectSendResult) {
                //成功的处理
                if (0 == PRINT_LOG_INTEVAL.incrementAndGet() % 500) {
                    log.info("发送消息成功  *** 发送消息成功 *** 发送消息成功的异步回调，topic = 【{}】", topic);
                }
            }
        });
    }
}
