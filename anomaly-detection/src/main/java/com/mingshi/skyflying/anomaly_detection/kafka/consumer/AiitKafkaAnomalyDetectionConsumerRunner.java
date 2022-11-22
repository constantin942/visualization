package com.mingshi.skyflying.anomaly_detection.kafka.consumer;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * <B>方法名称：AiitKafkaAnomalyDetectionConsumerRunner</B>
 * <B>概要说明：异常检测，项目启动时，创建消费者线程</B>
 *
 * @Author zm
 * @Date 2022-10-17 14:04:58
 * @Param
 * @return
 **/
@Component
@Slf4j
public class AiitKafkaAnomalyDetectionConsumerRunner implements ApplicationRunner {
    @Value("${spring.kafka.consumer.servers}")
    private String bootstrapServers;
    @Value("${spring.kafka.anomaly-detection-alarm-topic}")
    private String anomalyDetectionAlarmTopic;
    @Value("${spring.kafka.anomaly-detection-consume-failed-topic}")
    private String anomalyDetectionConsumeFailedTopic;
    @Value("${spring.kafka.consumer.anomaly-detection-consume-failed-and-alarm-group}")
    private String anomalyDetectionConsumeFailedAndAlarmGroup;
    @Resource
    private AnomalyDetectionBusiness anomalyDetectionBusiness;
    @Resource
    private MingshiServerUtil mingshiServerUtil;

    @Override
    public void run(ApplicationArguments args) throws Exception {
        try {
            // 创建MsKafkaSegmentsConsumer消费者线程，并启动
            createMsKafkaSegmentsConsumer();
        } catch (Exception e) {
            log.error("# AiitKafkaAnomalyDetectionConsumerRunner.run() # 创建并启动Kafka消费者出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：createMsKafkaSegmentsConsumer</B>
     * <B>概要说明：创建异常检测用的消费者线程，并启动</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 14:16:54
     * @Param []
     **/
    private void createMsKafkaSegmentsConsumer() {
        List<String> consumerTopicList = new ArrayList<>();
        consumerTopicList.add(anomalyDetectionAlarmTopic);
        consumerTopicList.add(anomalyDetectionConsumeFailedTopic);
        MsKafkaAnomalyDetectionConsumer msKafkaAnomalyDetectionConsumer = new MsKafkaAnomalyDetectionConsumer(anomalyDetectionBusiness, bootstrapServers, consumerTopicList, anomalyDetectionConsumeFailedAndAlarmGroup, mingshiServerUtil);
        msKafkaAnomalyDetectionConsumer.setName("aiit_kafka_anomaly_detection_consumer");
        msKafkaAnomalyDetectionConsumer.start();
    }
}
