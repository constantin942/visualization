package com.mingshi.web.controller;

import com.mingshi.skyflying.common.kafka.producer.AiitKafkaProducer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * @Author zhaoming
 * @Description 不需要登录校验的接口写这里面
 * @Date 15:28 2020/2/2
 * @Param
 * @return
 **/
@RestController
@Slf4j
@RequestMapping("/api/test")
public class TestController {
  @Resource
  private AiitKafkaProducer aiitKafkaProducer;

  @GetMapping(value = "/sendMsg")
  public void testSendMsgToKafka(String topic){
    for (int i = 0; i < 1; i++) {
      System.out.println("开始发送第" + (i + 1) + "条消息：" + i);
      aiitKafkaProducer.send(topic,i + "");
    }
  }
}
