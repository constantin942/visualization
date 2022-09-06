package com.mingshi.skyflying.common.redis;

import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.Config;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.IOException;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/30
 */
@Configuration
public class RedissonConfig {
    @Value("${spring.redis.host}")
    private String HOST;
    @Value("${spring.redis.port}")
    private String PORT;
    @Value("${spring.redis.password}")
    private String PASSWORD;

    @Bean(destroyMethod = "shutdown")
    public RedissonClient redisson() throws IOException {
        Config config = new Config();
        config.useSingleServer().setAddress("redis://" + HOST + ":" + PORT).setPassword(PASSWORD);
        return Redisson.create(config);
    }
}
