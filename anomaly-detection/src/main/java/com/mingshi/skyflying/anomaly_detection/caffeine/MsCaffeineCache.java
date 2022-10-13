package com.mingshi.skyflying.anomaly_detection.caffeine;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/10/13
 */

@Slf4j
@Component
public class MsCaffeineCache implements ApplicationRunner {

    private static Cache<String, String> redisLocalCache = null;
    @Override
    public void run(ApplicationArguments args) throws Exception {
        try{
            log.info("# MsCaffeineCache.run() # 项目启动，开始初始化Caffeine实例。");
            createAllCaffeine();
            log.info("# MsCaffeineCache.run() # 项目启动，初始化Caffeine实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.run() # 项目启动，初始化Caffeine实例时，出现了异常.", e);
        }
    }

    private static void createAllCaffeine() {
        createRedisLocalCache();
    }

    private static void createRedisLocalCache() {
        redisLocalCache = Caffeine.newBuilder()
                .expireAfterWrite(AnomalyConst.LOCAL_REDIS_CACHE_EXPIRE, TimeUnit.MINUTES)
                .maximumSize(AnomalyConst.LOCAL_REDIS_CACHE_SIZE)
                .build();
    }

    public static String getFromRedisLocalCache(String key) {
        return redisLocalCache.getIfPresent(key);
    }

    public static void putIntoRedisLocalCache(String key, String value) {
        redisLocalCache.put(key, value);
    }

    public static Cache<String, String> getRedisLocalCache() {
        return redisLocalCache;
    }
}
