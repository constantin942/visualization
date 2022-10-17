package com.mingshi.web;

import lombok.extern.log4j.Log4j2;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.bind.annotation.RestController;

@EnableScheduling
@Log4j2
@RestController
@EnableAsync
@SpringBootApplication
// 将数据库连接池里的事务交给springboot管理；2022-09-05 17:51:18
// @EnableTransactionManagement
@ComponentScan(basePackages = {"com.mingshi.skyflying.*","com.mingshi.web.*","com.mingshi.skyflying.common.*"})
@MapperScan({"com.mingshi.skyflying.dao", "com.mingshi.skyflying.anomaly_detection.dao","com.mingshi.skyflying.common.dao"})
public class SkywalkingMingshiServerApplication {

    public static void main(String[] args) {
        SpringApplication.run(SkywalkingMingshiServerApplication.class, args);
    }

}
