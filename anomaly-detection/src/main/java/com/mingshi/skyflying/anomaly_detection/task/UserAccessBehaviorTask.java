package com.mingshi.skyflying.anomaly_detection.task;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTableMapper;
import com.mingshi.skyflying.anomaly_detection.utils.AnomylyDetectionUtil;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.UserCoarseInfo;
import com.mingshi.skyflying.common.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;

/**
 * <B>类名称：UserAccessTaskBehavior</B>
 * <B>概要说明：获取用户访问行为的定时任务</B>
 *
 * @Author zm
 * @Date 2022/11/17 14:09
 **/
@Slf4j
@Configuration
@EnableScheduling
public class UserAccessBehaviorTask {

    @Resource
    RedisPoolUtil redisPoolUtil;

    @Resource
    UserPortraitByTableMapper userPortraitByTableMapper;

    @Resource
    AnomylyDetectionUtil anomylyDetectionUtil;

    /**
     * <B>方法名称：scheduledUserAccessBehaviorTask</B>
     * <B>概要说明：每间隔5秒钟，从Redis中获取用户的最近访问时间、对系统总的访问次数、访问次数最多的表</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-17 14:28:56
     * @Param []
     **/
    //间隔5秒执行
    @Scheduled(cron = "0/5 * * * * ? ")
    @Async
    public void scheduledUserAccessBehaviorTask() {
        Instant now = Instant.now();
        try {
            Integer period = anomylyDetectionUtil.getPeriod();
            if (period.equals(-Const.NUMBER_ONE)) {
                log.error("# UserAccessBehaviorTask.scheduledUserAccessBehaviorTask() # 定时从Redis中获取用户的访问信息时，没有从数据库中获取到用户的画像配置信息。");
                return;
            }

            Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
            queryMap.put(Const.PERIOD, period);

            // 从数据库中获取所有的用户名；
            List<String> users = userPortraitByTableMapper.getAllUser(queryMap);
            for (String user : users) {
                // 从Redis中获取用户最近的访问时间；2022-11-17 14:28:32
                String lastVisitedDate = (String) redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + user);
                if (StringUtil.isNotBlank(lastVisitedDate)) {
                    MsCaffeineCache.setUserAccessTaskBehaviorCache(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + user, lastVisitedDate);
                }

                // 从Redis中获取用户对系统总的访问次数；2022-11-17 14:28:32
                Object obj = redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + user);
                if (null != obj) {
                    MsCaffeineCache.setUserAccessTaskBehaviorCache(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + user, obj);
                }

                String tableName = null;
                // 从Redis中获取指定用户访问次数最多的表；2022-11-17 14:28:32
                Set<String> set = redisPoolUtil.reverseRange(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + user, 0L, 0L);
                if (null != set && !set.isEmpty()) {
                    Object[] objects = set.toArray();
                    tableName = String.valueOf(objects[0]);
                    MsCaffeineCache.setUserAccessTaskBehaviorCache(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + user, tableName);
                }
            }
        } catch (Exception e) {
            log.error("# UserAccessBehaviorTask.scheduledUserAccessBehaviorTask() # 定时从Redis中获取用户的配置信息时，出现了异常。", e);
        }
        log.info("# UserAccessBehaviorTask.scheduledUserAccessBehaviorTask() # 定时从Redis中获取用户的访问信息完毕，耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
    }
}
