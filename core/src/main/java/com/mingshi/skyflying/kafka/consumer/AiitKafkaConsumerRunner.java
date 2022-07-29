package com.mingshi.skyflying.kafka.consumer;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: AiitKafkaConsumerRunner</B>
 * <B>概要说明：启动kafka的消费者线程</B>
 * Author zm
 * Date 2022/7/28 17:25
 *
 * @Version 1.0
 **/
@Component
@Slf4j
public class AiitKafkaConsumerRunner implements ApplicationRunner {
  @Value("${spring.kafka.bootstrap-servers}")
  private String bootstrapServers;
  @Resource
  private AiitKafkaConsumerUtil aiitKafkaConsumerUtil;

  @Override
  public void run(ApplicationArguments args) throws Exception {
    try {
      while(true){
        if(null != aiitKafkaConsumerUtil){
          break;
        }else{
          TimeUnit.MILLISECONDS.sleep(50);
          log.error("# AiitKafkaConsumerRunner.run() # 项目启动，创建并启动Kafka消费者时，暂时还没有获取到 AiitKafkaConsumerUtil 实例信息。循环等待50毫秒。");
        }
      }
      MSKafkaConsumer MSKafkaConsumer = new MSKafkaConsumer(aiitKafkaConsumerUtil, bootstrapServers);
      MSKafkaConsumer.setName("aiit_kafka_consumer");
      MSKafkaConsumer.start();
    } catch (Exception e) {
      log.error("# AiitKafkaConsumerRunner.run() # 创建并启动Kafka消费者出现了异常。", e);
    }
  }
}
