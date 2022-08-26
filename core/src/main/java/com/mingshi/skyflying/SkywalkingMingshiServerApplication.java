package com.mingshi.skyflying;

import lombok.extern.log4j.Log4j2;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.bind.annotation.RestController;

@EnableScheduling
@Log4j2
@RestController
@SpringBootApplication
@ComponentScan(basePackages = {"com.mingshi.skyflying.*"})
@MapperScan({"com.mingshi.skyflying.dao", "com.mingshi.skyflying.anomaly_detection.dao"})
public class SkywalkingMingshiServerApplication {

    public static void main(String[] args) {
        SpringApplication.run(SkywalkingMingshiServerApplication.class, args);
    }

}
