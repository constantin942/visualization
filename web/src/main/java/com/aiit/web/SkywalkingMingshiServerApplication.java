package com.aiit.web;

import lombok.extern.log4j.Log4j2;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.web.bind.annotation.RestController;

@EnableScheduling
@Log4j2
@RestController
@EnableAsync
@SpringBootApplication
/**
 * 将数据库连接池里的事务交给springboot管理；2022-09-05 17:51:18
 */
@EnableTransactionManagement
@ComponentScan(basePackages = {"com.aiit.skyflying.*","com.aiit.web.*","com.aiit.skyflying.common.*"})
@MapperScan({"com.aiit.skyflying.dao", "com.aiit.skyflying.anomaly_detection.dao","com.aiit.skyflying.common.dao"})
public class SkywalkingMingshiServerApplication {

    public static void main(String[] args) {
        SpringApplication.run(SkywalkingMingshiServerApplication.class, args);
    }

}
