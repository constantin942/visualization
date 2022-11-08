package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.utils.AiitKafkaConsumerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: AiitKafkaConsumerRunner</B>
 * <B>概要说明：启动kafka的消费者线程</B>
 *
 * @Author zm
 * Date 2022/7/28 17:25
 * @Version 1.0
 **/
@Component
@Slf4j
public class AiitKafkaConsumerRunner implements ApplicationRunner {
    @Value("${spring.kafka.bootstrap-servers}")
    private String bootstrapServers;
    @Value("${spring.kafka.consumer.topic}")
    private String segmentConsumerTopic;
    @Value("${spring.kafka.consumer.agent-switch-response-topic}")
    private String agentSwitchResponseTopic;
    @Value("${spring.kafka.consumer.send-state-exception-info-topic}")
    private String sendStateExceptionInfoTopic;
    @Value("${spring.kafka.consumer.group}")
    private String segmentConsumerGroup;

    @Resource
    private AiitKafkaConsumerUtil aiitKafkaConsumerUtil;

    @Override
    public void run(ApplicationArguments args) throws Exception {
        try {
            while (true) {
                if (null != aiitKafkaConsumerUtil) {
                    break;
                } else {
                    TimeUnit.MILLISECONDS.sleep(50);
                    log.error("# AiitKafkaConsumerRunner.run() # 项目启动，创建并启动Kafka消费者时，暂时还没有获取到 AiitKafkaConsumerUtil 实例信息。循环等待50毫秒。");
                }
            }
            // 创建MsKafkaSegmentsConsumer消费者线程，并启动
            createMsKafkaSegmentsConsumer();
            // 创建createMsKafkaConsumer消费者线程，并启动
            createMsKafkaConsumer();
        } catch (Exception e) {
            log.error("# AiitKafkaConsumerRunner.run() # 创建并启动Kafka消费者出现了异常。", e);
        }
    }

    private void createMsKafkaConsumer() {
        List<String> topicList = new LinkedList<>();
        topicList.add(agentSwitchResponseTopic);
        topicList.add(sendStateExceptionInfoTopic);

        MsKafkaConsumer msKafkaConsumer = new MsKafkaConsumer(bootstrapServers, topicList, "send-state-exception-info-group", aiitKafkaConsumerUtil);
        msKafkaConsumer.setName("msKafkaSendStateAndExceptionConsumer");
        msKafkaConsumer.start();
    }

    /**
     * <B>方法名称：createMsKafkaSegmentsConsumer</B>
     * <B>概要说明：创建MsKafkaSegmentsConsumer消费者线程，并启动</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月25日 11:08:01
     * @Param []
     **/
    private void createMsKafkaSegmentsConsumer() {
        MsKafkaSegmentsConsumer msKafkaSegmentsConsumer = new MsKafkaSegmentsConsumer(aiitKafkaConsumerUtil, bootstrapServers, segmentConsumerTopic, segmentConsumerGroup);
        msKafkaSegmentsConsumer.setName("aiit_kafka_consumer");
        msKafkaSegmentsConsumer.start();
    }
}
