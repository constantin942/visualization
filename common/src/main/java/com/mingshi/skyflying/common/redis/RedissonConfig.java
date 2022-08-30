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
    private static final String HOST = "10.0.107.46";
    @Value("${spring.redis.port}")
    private static final String PORT = "6380";
    @Value("${spring.redis.password}")
    private static final String PASSWORD = "Aiit@123";

    @Bean(destroyMethod = "shutdown")
    public RedissonClient redisson() throws IOException {
        Config config = new Config();
        config.useSingleServer().setAddress("redis://" + HOST + ":" + PORT).setPassword(PASSWORD);
        return Redisson.create(config);
    }
}
