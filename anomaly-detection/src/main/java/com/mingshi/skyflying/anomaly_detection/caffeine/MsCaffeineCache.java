package com.mingshi.skyflying.anomaly_detection.caffeine;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitTask;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
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

    @Resource
    PortraitConfigMapper portraitConfigMapper;

    /**
     * 基于库表用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTableLocalCache = null;
    private static AtomicBoolean userPortraitByTableLocalCacheIsReady = new AtomicBoolean(Boolean.FALSE);

    /**
     * 存放用户最近访问时间、访问次数最多的表、访问系统总次数的缓存；
     */
    private static Cache<String, Object> userAccessTaskBehaviorCache = null;

    public static void setUserAccessTaskBehaviorCache(String key, Object value) {
        if(null != userAccessTaskBehaviorCache){
            userAccessTaskBehaviorCache.put(key, value);
        }
    }

    public static Object getUserAccessTaskBehaviorCache(String key){
        if(null != userAccessTaskBehaviorCache){
            return userAccessTaskBehaviorCache.getIfPresent(key);
        }
        return null;
    }

    /**
     * 存储用户画像配置信息；2022-11-17 13:37:07
     */
    private static Cache<String, PortraitConfig> portraitConfigSelectOne = null;

    public static PortraitConfig getPortraitConfigSelectOne(String key) {
        if(null == portraitConfigSelectOne){
            return null;
        }
        return portraitConfigSelectOne.getIfPresent(key);
    }

    /**
     * 将画像配置信息存到本地缓存；2022-11-17 13:51:37
     *
     * @param key
     * @param value
     */
    public static void setPortraitConfigSelectOne(String key, PortraitConfig value) {
        if(null == portraitConfigSelectOne){
            return;
        }
        portraitConfigSelectOne.put(key, value);
    }

    /**
     * <B>方法名称：getUserPortraitByTableLocalCacheIsReady</B>
     * <B>概要说明：判断缓存userPortraitByTableLocalCache是否已建立成功</B>
     *
     * @return java.lang.Boolean
     * @Author zm
     * @Date 2022-11-07 14:59:11
     * @Param []
     **/
    public static Boolean getUserPortraitByTableLocalCacheIsReady() {
        return userPortraitByTableLocalCacheIsReady.get();
    }

    /**
     * 基于时间小时时段用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTimeLocalCache = null;
    private static AtomicBoolean userPortraitByTimeLocalCacheIsReady = new AtomicBoolean(Boolean.FALSE);

    /**
     * <B>方法名称：getUserPortraitByTimeLocalCacheIsReady</B>
     * <B>概要说明：判断缓存userPortraitByTimeLocalCacheIsReady是否已建立成功</B>
     *
     * @return java.lang.Boolean
     * @Author zm
     * @Date 2022-11-07 14:59:11
     * @Param []
     **/
    public static Boolean getUserPortraitByTimeLocalCacheIsReady() {
        return userPortraitByTimeLocalCacheIsReady.get();
    }

    /**
     * 基于时间早中晚分区用户画像本地缓存
     */
    private static Cache<String, String> userPortraitByTimePartitionLocalCache = null;
    private static AtomicBoolean userPortraitByTimePartitionLocalCacheIsReady = new AtomicBoolean(Boolean.FALSE);

    /**
     * <B>方法名称：getUserPortraitByTimePartitionLocalCacheIsReady</B>
     * <B>概要说明：判断缓存userPortraitByTimePartitionLocalCache是否已建立成功</B>
     *
     * @return java.lang.Boolean
     * @Author zm
     * @Date 2022-11-07 14:59:11
     * @Param []
     **/
    public static Boolean getUserPortraitByTimePartitionLocalCacheIsReady() {
        return userPortraitByTimePartitionLocalCacheIsReady.get();
    }

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
    private static AtomicBoolean userPortraitInitDone = new AtomicBoolean(Boolean.FALSE);
    /**
     * 钉钉通知信息插入完毕标识；
     */
    private static AtomicBoolean dingInfoInsertedDone = new AtomicBoolean(Boolean.TRUE);

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
     * @return void
     * @Author zm
     * @Date 2022-10-19 14:10:02
     * @Param [flag]
     **/
    public static void setUserPortraitInitDone(Boolean flag) {
        userPortraitInitDone.set(flag);
    }

    /**
     * <B>方法名称：getDingInfoInsertedDone</B>
     * <B>概要说明：获取钉钉信息插入完毕标识</B>
     *
     * @return java.lang.Boolean
     * @Author lyx
     * @Date 2022-11-09 12:25:28
     * @Param []
     **/
    public static Boolean getDingInfoInsertedDone() {
        return dingInfoInsertedDone.get();
    }

    /**
     * <B>方法名称：setDingInfoInsertedDone</B>
     * <B>概要说明：设置钉钉信息插入是否完毕的标识</B>
     *
     * @return void
     * @Author lyx
     * @Date 2022-11-09 12:30:02
     * @Param [flag]
     **/
    public static void setDingInfoInsertedDone(Boolean flag) {
        dingInfoInsertedDone.set(flag);
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
        // 创建基于时间早中晚分区用户画像本地缓存
        createPortraitByTimePartitionLocalCache();
        // 创建基于时间小时用户画像本地缓存
        createPortraitByTimeLocalCache();
        // 创建用户首次访问时间本地缓存
        createFirstVisitTimeLocalCache();
        // 创建告警抑制本地缓存
        createAlarmInhibitLocalCache();
        // 创建告警抑制本地缓存
        createPortraitConfigSelectOne();
        // 创建存储用户访问行为的缓存
        createUserAccessTaskBehaviorCache();
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
            userPortraitByTableLocalCacheIsReady.set(Boolean.TRUE);
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createPortraitByTableLocalCache() # 项目启动，初始化userPortraitByTableLocalCache实例时，出现了异常.", e);
        }
    }

    /**
     * <B>方法名称：createPortraitConfigSelectOne</B>
     * <B>概要说明：创建用户画像配置的缓存</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-17 13:41:42
     * @Param []
     **/
    private static void createPortraitConfigSelectOne() {
        try {
            log.info("# MsCaffeineCache.createPortraitConfigSelectOne() # 项目启动，开始初始化portraitConfigSelectOne实例。");
            portraitConfigSelectOne = Caffeine.newBuilder()
                .expireAfterWrite(AnomalyConst.LOCAL_REDIS_CACHE_EXPIRE, TimeUnit.MINUTES)
                .maximumSize(AnomalyConst.PORTRAIT_CONFIG_SELECT_ONE)
                .build();
            log.info("# MsCaffeineCache.createPortraitConfigSelectOne() # 项目启动，初始化portraitConfigSelectOne实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createPortraitConfigSelectOne() # 项目启动，初始化portraitConfigSelectOne实例时，出现了异常.", e);
        }
    }

    /**
     * <B>方法名称：createUserAccessTaskBehaviorCache</B>
     * <B>概要说明：创建存储用户访问行为的缓存</B>
     *
     * @Author zm
     * @Date 2022-11-17 14:14:31
     * @Param []
     * @return void
     **/
    private static void createUserAccessTaskBehaviorCache() {
        try {
            log.info("# MsCaffeineCache.createUserAccessTaskBehaviorCache() # 项目启动，开始初始化userAccessTaskBehaviorCache实例。");
            userAccessTaskBehaviorCache = Caffeine.newBuilder()
                .expireAfterWrite(AnomalyConst.USER_ACCESS_TASK_BEHAVIOR_CACHE_EXPIRE, TimeUnit.SECONDS)
                .maximumSize(AnomalyConst.USER_ACCESS_TASK_BEHAVIOR_CACHE)
                .build();
            log.info("# MsCaffeineCache.createUserAccessTaskBehaviorCache() # 项目启动，初始化userAccessTaskBehaviorCache实例完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createUserAccessTaskBehaviorCache() # 项目启动，初始化userAccessTaskBehaviorCache实例时，出现了异常.", e);
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
            userPortraitByTimeLocalCacheIsReady.set(Boolean.TRUE);
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createPortraitByTimeLocalCache() # 项目启动，初始化userPortraitByTimeLocalCache实例时，出现了异常.", e);
        }
    }


    /**
     * <B>方法名称：createPortraitByTimeLocalCache</B>
     * <B>概要说明：创建基于时间分区用户画像本地缓存</B>
     *
     * @return void
     * @Author lyx
     * @Date 2022-11-04 23:01:56
     * @Param []
     **/
    private static void createPortraitByTimePartitionLocalCache() {
        try {
            log.info("# MsCaffeineCache.createPortraitByTimePartitionLocalCache() # 项目启动，开始初始化userPortraitByTimePartitionLocalCache实例。");
            userPortraitByTimePartitionLocalCache = Caffeine.newBuilder()
                .expireAfterAccess(AnomalyConst.USER_PORTRAIT_LOCAL_CACHE_EXPIRE, TimeUnit.HOURS)
                .maximumSize(AnomalyConst.USER_PORTRAIT_TIME_PARTITION_LOCAL_CACHE_SIZE)
                .build();
            log.info("# MsCaffeineCache.createPortraitByTimePartitionLocalCache() # 项目启动，初始化userPortraitByTimePartitionLocalCache实例完毕。");
            userPortraitByTimePartitionLocalCacheIsReady.set(Boolean.TRUE);
        } catch (Exception e) {
            log.error("# MsCaffeineCache.createPortraitByTimePartitionLocalCache() # 项目启动，初始化userPortraitByTimePartitionLocalCache实例时，出现了异常.", e);
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

    public static String getFromPortraitByTimePartitionLocalCache(String key) {
        return userPortraitByTimePartitionLocalCache.getIfPresent(key);
    }

    public static void putIntoPortraitByTimePartitionLocalCache(String key, String value) {
        userPortraitByTimePartitionLocalCache.put(key, value);
    }

    public static void putAllIntoPortraitByTimePartitionLocalCache(Map<String, String> map) {
        userPortraitByTimePartitionLocalCache.putAll(map);
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
