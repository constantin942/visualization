package com.mingshi.skyflying.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * <B>主类名称: StartKafkaConsumer</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/4/20 14:49
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class StartKafkaConsumer{
  // @PostConstruct
  // public void run(){
  //   log.info("开始执行 StartKafkaConsumer # run()方法，启动kafka的消费者。");
  //   Consumer consumer = new Consumer("skywalking-segments");
  //   consumer.start();
  // }
}
