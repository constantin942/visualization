package com.mingshi.skyflying.anomaly_detection.caffeine;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitTask;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

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

    @Resource
    UserPortraitTask userPortraitTask;
    /**
     * 基于库表用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTableLocalCache = null;

    /**
     * 基于时间用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTimeLocalCache = null;
    /**
     * 用户首次访问时间本地缓存
     */
    private static Cache<String, Date> userFirstVisitLocalCache = null;

    /**
     * 配置信息缓存
     */
    private static ConfigCache configCache = new ConfigCache();
    /**
     * 告警抑制缓存
     */
    private static Cache<String, Instant> alarmInhibitCache = null;
    /**
     * 用户画像相关信息初始化完毕标识；
     */
    private static volatile AtomicBoolean userPortraitInitDone = new AtomicBoolean(Boolean.FALSE);

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
        return userPortraitInitDone.get();
    }

    /**
     * <B>方法名称：setUserPortraitInitDone</B>
     * <B>概要说明：设置用户画像信息初始化是否完毕的标识</B>
     *
     * @Author zm
     * @Date 2022-10-19 14:10:02
     * @Param [flag]
     * @return void
     **/
    public static void setUserPortraitInitDone(Boolean flag) {
        userPortraitInitDone.set(flag);
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        createAllCaffeine();
        initConfigCache();
        initFirstVisitTime();
        initUserPortraitLocalCache();
    }

    /**
     * 缓存用户首次访问时间
     */
    private void initFirstVisitTime() {
        userPortraitTask.initFirstVisitTime(userFirstVisitLocalCache);
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
        Boolean portraitFromRedis = anomalyDetectionBusiness.getPortraitFromRedis();
        if (Boolean.TRUE.equals(portraitFromRedis)) {
            userPortraitInitDone.set(true);
        }
    }

    private static void createAllCaffeine() {
        // 创建基于库表用户画像本地缓存；2022-10-17 09:27:16
        createPortraitByTableLocalCache();
        // 创建基于时间用户画像本地缓存
        createPortraitByTimeLocalCache();
        // 创建用户首次访问时间本地缓存
        createFirstVisitTimeLocalCache();
        // 创建告警抑制本地缓存
        createAlarmInhibitLocalCache();
    }

    private static void createAlarmInhibitLocalCache() {
        try {
            log.info("# MsCaffeineCache.createAlarmInhibitLocalCache() # 项目启动，开始初始化alarmInhibitCache实例。");
            alarmInhibitCache = Caffeine.newBuilder()
                    .maximumSize(AnomalyConst.ALARM_INHIBIT_LOCAL_CACHE_SIZE)
                    .build();
            log.info("# MsCaffeineCache.createAlarmInhibitLocalCache() # 项目启动，初始化alarmInhibitCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createAlarmInhibitLocalCache() # 项目启动，初始化alarmInhibitCache实例时，出现了异常.", e);
        }
    }

    private static void createFirstVisitTimeLocalCache() {
        try {
            log.info("# MsCaffeineCache.createFirstVisitTimeLocalCache() # 项目启动，开始初始化userFirstVisitLocalCache实例。");
            userFirstVisitLocalCache = Caffeine.newBuilder()
                    .maximumSize(AnomalyConst.USER_FIRST_VISIT_LOCAL_CACHE_SIZE)
                    .build();
            log.info("# MsCaffeineCache.createFirstVisitTimeLocalCache() # 项目启动，初始化userFirstVisitLocalCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createFirstVisitTimeLocalCache() # 项目启动，初始化userFirstVisitLocalCache实例时，出现了异常.", e);
        }
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

    public static Instant getFromAlarmInhibitCache(String key) {
        return alarmInhibitCache.getIfPresent(key);
    }
    public static void putIntoAlarmInhibitCache(String key, Instant date) {
        alarmInhibitCache.put(key, date);
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

    public static PortraitConfig getPortraitConfig() {
        return configCache.portraitConfig;
    }

    public static Date getFromFirstVisitTime(String username) {
        return userFirstVisitLocalCache.getIfPresent(username);
    }

    public static Cache<String, Date> getUserFirstVisitLocalCache() {
        return userFirstVisitLocalCache;
    }
}
