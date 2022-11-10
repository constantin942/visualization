package com.mingshi.skyflying.anomaly_detection.task;

import com.github.benmanes.caffeine.cache.Cache;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.caffeine.ConfigCache;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.Resource;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：UserPortraitTask</B>
 * <B>概要说明：定时任务类</B>
 *
 * @Author zm
 * @Date 2022-10-17 13:35:50
 * @Param
 * @return
 **/
@Slf4j
@Configuration
@EnableScheduling
public class UserPortraitTask {

    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    @Resource
    MsSegmentDetailMapper segmentDetailMapper;


    /**
     * <B>方法名称：fetchUserPortrait</B>
     * <B>概要说明：定时5分钟拉取用户画像。这个方法是每个实例都需要执行的，所以不需要获取分布式锁。</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 11:26:13
     * @Param []
     **/
    @Scheduled(cron = "0 */5 * * * ?")
    @Async
    public void fetchUserPortrait() {
        try {
            // 定时拉取用户画像；
            Boolean portraitFromRedis = anomalyDetectionBusiness.getPortraitFromRedis();
            if (Boolean.TRUE.equals(portraitFromRedis)) {
                MsCaffeineCache.setUserPortraitInitDone(Boolean.TRUE);
            }
        } catch (Exception e) {
            log.error("定时拉取用户画像失败");
        }
    }


    @Scheduled(cron = "0 0 2 * * ?")
    @Async
    public void fetchPortraitConfigAndFirstTime() {
        try {
            // 定时拉取画像配置信息以及用户首次访问时间；
            ConfigCache configCache = MsCaffeineCache.getConfigCache();
            configCache.setPortraitConfig(anomalyDetectionBusiness.getConfig());
            configCache.setEnableTimeRule(anomalyDetectionBusiness.getEnableRule(AnomalyConst.TIME_SUF));
            configCache.setEnableTableRule(anomalyDetectionBusiness.getEnableRule(AnomalyConst.TABLE_SUF));
            initFirstVisitTime(MsCaffeineCache.getUserFirstVisitLocalCache());
        } catch (Exception e) {
            log.error("定时拉取画像配置信息以及用户首次访问时间失败");
        }
    }


    /**
     * 将每个用户的首次访问时间存入cache
     *
     * @param userFirstVisitLocalCache
     */
    public void initFirstVisitTime(Cache<String, Date> userFirstVisitLocalCache) {
        List<Map<String, String>> maps = segmentDetailMapper.selectFirstVisitTime();
        for (Map<String, String> map : maps) {
            try {
                userFirstVisitLocalCache.put(map.get(AnomalyConst.USER_NAME)
                        , new SimpleDateFormat(DateTimeUtil.STANDARD_FORMAT).parse(String.valueOf(map.get(AnomalyConst.TIME_SUF))));
            } catch (Exception e) {
                log.error("时间格式转换失败");
            }
        }
    }
}
