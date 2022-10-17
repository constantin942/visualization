package com.mingshi.skyflying.anomaly_detection.task;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.caffeine.ConfigCache;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTableMapper;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.Resource;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

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
    RedisPoolUtil redisPoolUtil;

    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;


    /**
     * <B>方法名称：fetchUserPortrait</B>
     * <B>概要说明：定时5分钟拉取用户画像。这个方法是每个实例都需要执行的，所以不需要获取分布式锁。</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 11:26:13
     * @Param []
     **/
    @Scheduled(cron = "0 0 1 * * ?")
    @Async
    public void fetchUserPortrait() {
        try {
            // 定时拉取用户画像；
            anomalyDetectionBusiness.getPortraitFromRedis();
        } catch (Exception e) {
            log.error("定时拉取用户画像失败");
        }
    }


    @Scheduled(cron = "0 0 2 * * ?")
    @Async
    public void fetchPortraitConfig() {
        try {
            // 定时拉取画像配置信息；
            ConfigCache configCache = MsCaffeineCache.getConfigCache();
            configCache.setPortraitConfig(anomalyDetectionBusiness.getConfig());
            configCache.setEnableTimeRule(anomalyDetectionBusiness.getEnableRule(AnomalyConst.TIME_SUF));
            configCache.setEnableTableRule(anomalyDetectionBusiness.getEnableRule(AnomalyConst.TABLE_SUF));
        } catch (Exception e) {
            log.error("定时拉取用户画像失败");
        }
    }
}
