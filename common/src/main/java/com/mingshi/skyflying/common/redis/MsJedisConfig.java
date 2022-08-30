package com.mingshi.skyflying.common.redis;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;

/**
 * 管理单个Jedis实例的组件
 */
@Slf4j
@Configuration
@PropertySource("classpath:application-${spring.profiles.active}.yml")
public class MsJedisConfig {

  @Bean
  public JedisPoolConfig jedisPoolConfig(@Value("${spring.redis.jedis.maxTotal}") int maxActive,
                                         @Value("${spring.redis.jedis.maxIdle}") int maxIdle,
                                         @Value("${spring.redis.jedis.minIdle}") int minIdle,
                                         @Value("${spring.redis.jedis.maxWaitMillis}") long maxWaitMillis,
                                         @Value("${spring.redis.jedis.testOnBorrow}") boolean testOnBorrow) {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMaxTotal(maxActive);
    jedisPoolConfig.setMaxIdle(maxIdle);
    jedisPoolConfig.setMinIdle(minIdle);
    jedisPoolConfig.setMaxWaitMillis(maxWaitMillis);
    jedisPoolConfig.setTestOnBorrow(testOnBorrow);

    return jedisPoolConfig;
  }

  @Bean
  public JedisPool jedisPool(@Value("${spring.redis.host}") String host,
                             @Value("${spring.redis.password}") String password,
                             @Value("${spring.redis.port}") int port,
                             @Value("${spring.redis.timeout}") int timeout, JedisPoolConfig jedisPoolConfig) {

    log.info("=====创建JedisPool连接池=====");
    if (StringUtils.isNotEmpty(password)) {
      return new JedisPool(jedisPoolConfig, host, port, timeout, password, 1);
    }

    return new JedisPool(jedisPoolConfig, host, port, timeout);
  }

}

