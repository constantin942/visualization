package com.mingshi.skyflying.anomaly_detection.utils;

import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Set;

/**
 * <B>类名称：AnomylyDetectionUtil</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022/11/17 14:18
 **/
@Component
public class AnomylyDetectionUtil {

    @Resource
    PortraitConfigMapper portraitConfigMapper;
    @Resource
    RedisPoolUtil redisPoolUtil;

    /**
     * <B>方法名称：getPeriod</B>
     * <B>概要说明：获取用户画像配置信息</B>
     *
     * @Author zm
     * @Date 2022-11-17 14:20:54
     * @Param []
     * @return java.lang.Integer
     **/
    public Integer getPeriod() {
        Integer period = -1;
        PortraitConfig portraitConfig = MsCaffeineCache.getPortraitConfigSelectOne(Const.PORTRAIT_CONFIG_SELECT_ONE);
        if(null == portraitConfig){
            portraitConfig = portraitConfigMapper.selectOne();
            if(null != portraitConfig){
                MsCaffeineCache.setPortraitConfigSelectOne(Const.PORTRAIT_CONFIG_SELECT_ONE, portraitConfig);
            }else{

            }
        }
        period = portraitConfig.getRuleTablePeriod();
        return period;
    }

    /**
     * <B>方法名称：getLastVisitedDate</B>
     * <B>概要说明：获取用户最近的访问时间</B>
     *
     * @Author zm
     * @Date 2022-11-17 14:30:44
     * @Param [key]
     * @return java.lang.String
     **/
    public String getLastVisitedDate(String key) {
        Object lastVisitedDate = MsCaffeineCache.getUserAccessTaskBehaviorCache(key);
        if(null == lastVisitedDate){
            // 从Redis中获取用户最近的访问时间；2022-11-17 14:28:32
            lastVisitedDate = redisPoolUtil.get(key);
            if(null != lastVisitedDate){
                MsCaffeineCache.setUserAccessTaskBehaviorCache(key, lastVisitedDate);
                return String.valueOf(lastVisitedDate);
            }
        }else{
            return String.valueOf(lastVisitedDate);
        }
        return null;
    }

    /**
     * <B>方法名称：getUserAllVisitedTimes</B>
     * <B>概要说明：获取用户访问系统总的次数</B>
     *
     * @Author zm
     * @Date 2022-11-17 14:37:12
     * @Param [key]
     * @return java.lang.Double
     **/
    public Double getUserAllVisitedTimes(String key) {
        Object userAllVisitedTimes = MsCaffeineCache.getUserAccessTaskBehaviorCache(key);
        if(null == userAllVisitedTimes){
            // 从Redis中获取用户最近的访问时间；2022-11-17 14:28:32
            userAllVisitedTimes = redisPoolUtil.get(key);
            if(null != userAllVisitedTimes){
                MsCaffeineCache.setUserAccessTaskBehaviorCache(key, userAllVisitedTimes);
                return Double.valueOf(String.valueOf(userAllVisitedTimes));
            }
        }else{
            return Double.valueOf(String.valueOf(userAllVisitedTimes));
        }
        return 0d;
    }

    /**
     * <B>方法名称：getUserVisitedTimesMostTable</B>
     * <B>概要说明：获取用户访问次数最多的表</B>
     *
     * @Author zm
     * @Date 2022-11-17 14:40:48
     * @Param [key]
     * @return java.lang.String
     **/
    public String getUserVisitedTimesMostTable(String key) {
        Object userVisitedTimesMostTable = MsCaffeineCache.getUserAccessTaskBehaviorCache(key);
        if(null == userVisitedTimesMostTable){
            // 从Redis中获取用户最近的访问时间；2022-11-17 14:28:32
            Set<String> set = redisPoolUtil.reverseRange(key, 0L, 0L);
            if (null != set && !set.isEmpty()) {
                Object[] objects = set.toArray();
                userVisitedTimesMostTable = objects[0];
            }
            if(null != userVisitedTimesMostTable){
                MsCaffeineCache.setUserAccessTaskBehaviorCache(key, userVisitedTimesMostTable);
                return String.valueOf(userVisitedTimesMostTable);
            }
        }else{
            return String.valueOf(userVisitedTimesMostTable);
        }
        return null;
    }
}
