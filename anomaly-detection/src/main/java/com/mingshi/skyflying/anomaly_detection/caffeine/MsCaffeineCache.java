package com.mingshi.skyflying.anomaly_detection.caffeine;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.units.qual.N;
import org.redisson.spring.cache.CacheConfig;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/10/13
 */

@Slf4j
@Component
public class MsCaffeineCache implements ApplicationRunner {

    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    private static Cache<String, String> redisLocalCache = null;

    /**
     * 基于库表用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTableLocalCache = null;

    /**
     * 基于时间用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTimeLocalCache = null;

    private static ConfigCache configCache = null;
    /**
     * 用户画像相关信息初始化完毕标识；
     */
    private static volatile Boolean userPortraitInitDone = false;

    /**
     * <B>方法名称：getUserPortraitInitDone</B>
     * <B>概要说明：获取用户画像信息初始化完毕标识</B>
     *
     * @return java.lang.Boolean
     * @Author zm
     * @Date 2022-10-17 10:25:28
     * @Param []
     **/
    public static Boolean getUserPortraitInitDone() {
        return userPortraitInitDone;
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        createAllCaffeine();
        initConfigCache();
        initUserPortraitLocalCache();
    }

    /**
     * 创建配置类缓存
     */
    private void initConfigCache() {
        configCache.setPortraitConfig(anomalyDetectionBusiness.getConfig());
        configCache.setEnableTimeRule(anomalyDetectionBusiness.getEnableRule(AnomalyConst.TIME_SUF));
        configCache.setEnableTableRule(anomalyDetectionBusiness.getEnableRule(AnomalyConst.TABLE_SUF));
    }

    /**
     * <B>方法名称：initUserPortraitLocalCache</B>
     * <B>概要说明：从Redis中拉取用户画像，并初始化本地缓存</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 09:43:58
     * @Param []
     **/
    private void initUserPortraitLocalCache() {
        // 当项目启动时，加载用户画像信息；
        anomalyDetectionBusiness.getPortraitFromRedis();
        userPortraitInitDone = true;
    }

    private static void createAllCaffeine() {
        // 创建基于库表用户画像本地缓存；2022-10-17 09:27:16
        createPortraitByTableLocalCache();
        // 创建基于时间用户画像本地缓存
        createPortraitByTimeLocalCache();
    }


    /**
     * <B>方法名称：createPortraitByTableLocalCache</B>
     * <B>概要说明：创建基于库表用户画像本地缓存</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 09:27:21
     * @Param []
     **/
    private static void createPortraitByTableLocalCache() {
        try {
            log.info("# MsCaffeineCache.createPortraitByTableLocalCache() # 项目启动，开始初始化userPortraitByTableLocalCache实例。");
            userPortraitByTableLocalCache = Caffeine.newBuilder()
                    .expireAfterAccess(AnomalyConst.USER_PORTRAIT_LOCAL_CACHE_EXPIRE, TimeUnit.HOURS)
                    .maximumSize(AnomalyConst.USER_PORTRAIT_TABLE_LOCAL_CACHE_SIZE)
                    .build();
            log.info("# MsCaffeineCache.createPortraitByTableLocalCache() # 项目启动，初始化userPortraitByTableLocalCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createPortraitByTableLocalCache() # 项目启动，初始化userPortraitByTableLocalCache实例时，出现了异常.", e);
        }
    }


    /**
     * <B>方法名称：createPortraitByTimeLocalCache</B>
     * <B>概要说明：创建基于时间用户画像本地缓存</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 09:27:21
     * @Param []
     **/
    private static void createPortraitByTimeLocalCache() {
        try {
            log.info("# MsCaffeineCache.createPortraitByTimeLocalCache() # 项目启动，开始初始化userPortraitByTimeLocalCache实例。");
            userPortraitByTimeLocalCache = Caffeine.newBuilder()
                    .expireAfterAccess(AnomalyConst.USER_PORTRAIT_LOCAL_CACHE_EXPIRE, TimeUnit.HOURS)
                    .maximumSize(AnomalyConst.USER_PORTRAIT_TIME_LOCAL_CACHE_SIZE)
                    .build();
            log.info("# MsCaffeineCache.createPortraitByTimeLocalCache() # 项目启动，初始化userPortraitByTimeLocalCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createPortraitByTimeLocalCache() # 项目启动，初始化userPortraitByTimeLocalCache实例时，出现了异常.", e);
        }
    }

    public static String getFromPortraitByTableLocalCache(String key) {
        return userPortraitByTableLocalCache.getIfPresent(key);
    }

    public static void putIntoPortraitByTableLocalCache(String key, String value) {
        userPortraitByTableLocalCache.put(key, value);
    }

    public static void putAllIntoPortraitByTableLocalCache(Map<String, String> map) {
        userPortraitByTableLocalCache.putAll(map);
    }

    public static String getFromPortraitByTimeLocalCache(String key) {
        return userPortraitByTimeLocalCache.getIfPresent(key);
    }

    public static void putIntoPortraitByTimeLocalCache(String key, String value) {
        userPortraitByTimeLocalCache.put(key, value);
    }

    public static void putAllIntoPortraitByTimeLocalCache(Map<String, String> map) {
        userPortraitByTimeLocalCache.putAll(map);
    }

    public static Cache<String, String> getUserPortraitByTimeLocalCache() {
        return userPortraitByTimeLocalCache;
    }
    public static ConfigCache getConfigCache() {
        return configCache;
    }

    public static Boolean getEnableTimeRule() {
        return configCache.enableTimeRule;
    }
    public static Boolean getEnableTableRule() {
        return configCache.enableTableRule;
    }
    public static  PortraitConfig getPortraitConfig() {
        return configCache.portraitConfig;
    }

}
