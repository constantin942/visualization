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

    /**
     * 用户画像本地缓存
     */
    private static Cache<String, String> userPortraitLocalCache = null;
    /**
     * 用户画像信息初始化完毕标识；
     */
    private static volatile Boolean userPortraitInitDone = false;

    /**
     * <B>方法名称：getUserPortraitInitDone</B>
     * <B>概要说明：获取用户画像信息初始化完毕标识</B>
     *
     * @Author zm
     * @Date 2022-10-17 10:25:28
     * @Param []
     * @return java.lang.Boolean
     **/
    public static Boolean getUserPortraitInitDone(){
        return userPortraitInitDone;
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        createAllCaffeine();
        initUserPortraitLocalCache();
    }

    /**
     * <B>方法名称：initUserPortraitLocalCache</B>
     * <B>概要说明：从Redis中拉取用户画像，并初始化本地缓存</B>
     *
     * @Author zm
     * @Date 2022-10-17 09:43:58
     * @Param []
     * @return void
     **/
    private void initUserPortraitLocalCache() {
        // TODO: 2022/10/17 当项目启动时，加载用户画像信息；


        userPortraitInitDone = true;
    }

    private static void createAllCaffeine() {
        createRedisLocalCache();
        // 创建用户画像本地缓存；2022-10-17 09:27:16
        createUserPortraitLocalCache();
    }

    private static void createRedisLocalCache() {
        try{
            log.info("# MsCaffeineCache.createRedisLocalCache() # 项目启动，开始初始化redisLocalCache实例。");
            redisLocalCache = Caffeine.newBuilder()
                .expireAfterWrite(AnomalyConst.LOCAL_REDIS_CACHE_EXPIRE, TimeUnit.MINUTES)
                .maximumSize(AnomalyConst.LOCAL_REDIS_CACHE_SIZE)
                .build();
            log.info("# MsCaffeineCache.createRedisLocalCache() # 项目启动，初始化redisLocalCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createRedisLocalCache() # 项目启动，初始化redisLocalCache实例时，出现了异常.", e);
        }
    }
    /**
     * <B>方法名称：createUserPortraitLocalCache</B>
     * <B>概要说明：创建用户画像本地缓存</B>
     *
     * @Author zm
     * @Date 2022-10-17 09:27:21
     * @Param []
     * @return void
     **/
    private static void createUserPortraitLocalCache() {
        try{
            log.info("# MsCaffeineCache.createUserPortraitLocalCache() # 项目启动，开始初始化userPortraitLocalCache实例。");
            userPortraitLocalCache = Caffeine.newBuilder()
                .expireAfterAccess(AnomalyConst.USER_PORTRAIT_LOCAL_CACHE_EXPIRE, TimeUnit.HOURS)
                .maximumSize(AnomalyConst.USER_PORTRAIT_LOCAL_CACHE_SIZE)
                .build();
            log.info("# MsCaffeineCache.createUserPortraitLocalCache() # 项目启动，初始化userPortraitLocalCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createUserPortraitLocalCache() # 项目启动，初始化userPortraitLocalCache实例时，出现了异常.", e);
        }
    }

    public static String getFromUserPortraitLocalCache(String key) {
        return userPortraitLocalCache.getIfPresent(key);
    }

    public static void putIntoUserPortraitLocalCache(String key, String value) {
        userPortraitLocalCache.put(key, value);
    }

    public static String getFromRedisLocalCache(String key) {
        return redisLocalCache.getIfPresent(key);
    }

    public static void putIntoRedisLocalCache(String key, String value) {
        redisLocalCache.put(key, value);
    }

    public static Cache<String, String> getUserPortraitLocalCache() {
        return userPortraitLocalCache;
    }

    public static Cache<String, String> getRedisLocalCache() {
        return redisLocalCache;
    }
}
