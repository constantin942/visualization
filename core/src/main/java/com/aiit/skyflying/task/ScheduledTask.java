package com.aiit.skyflying.task;

import com.aiit.skyflying.common.caffeine.MsCaffeine;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.utils.DateTimeUtil;
import com.aiit.skyflying.common.utils.MingshiServerUtil;
import com.aiit.skyflying.common.utils.RedisPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import javax.xml.transform.Templates;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: ScheduledTask</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * Date 2022/5/26 17:25
 * @Version 1.0
 **/
@Component
@Slf4j
public class ScheduledTask {
    @Resource
    private ExecitonScheduledTaskList execitonScheduledTaskList;
    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private MsCaffeine msCaffeine;
    @Resource
    private RedisPoolUtil redisPoolUtil;

    /**
     * <B>方法名称：scheduledDeleteTwoDaysBeforeSegmentDetailDo</B>
     * <B>概要说明：删除两天前的用户访问信息</B>
     *
     * @Author zm
     * @Date 2022-11-07 14:17:27
     * @Param
     * @return
     **/
    /**
     * 每隔30分钟执行一次：
     */
    @Scheduled(cron = "0 */20 * * * ?")
//    @Scheduled(cron = "0/10 * * * * ? ") //间隔60秒执行
    /**
     * 每隔2小时执行一次；
     */
    // @Scheduled(cron = "0 0 0/2 * * ?")
    public void scheduledDeleteTwoDaysBeforeSegmentDetailDo() {
        execitonScheduledTaskList.doScheduledDeleteTwoDaysBeforeSegmentDetailDo(Const.SCHEDULED_DELETE_TWO_DAYS_BEFORE_SEGMENT_DETAIL_DO);
    }

    /**
     * <B>方法名称：scheduledGetSegmentDetailDo</B>
     * <B>概要说明：定时从 ms_segment_detail_username_is_null 表中获取用户名不为空的记录</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 15:08:37
     * @Param []
     **/
    /**
     * 每隔30分钟执行一次：
     */
    @Scheduled(cron = "0 */20 * * * ?")
//    @Scheduled(cron = "0/10 * * * * ? ") //间隔60秒执行
    /**
     * 每隔2小时执行一次；
     */
    // @Scheduled(cron = "0 0 0/2 * * ?")
    public void scheduledGetSegmentDetailDo() {
        execitonScheduledTaskList.doScheduledGetSegmentDetailDo(Const.SCHEDULED_GET_SEGMENT_DETAIL_DO);
    }

    /**
     * <B>方法名称：scheduledUpdateUserNameByToken</B>
     * <B>概要说明：定时基于token更新用户名</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 15:08:36
     * @Param []
     **/
    // 每隔30分钟执行一次：
    @Scheduled(cron = "0 */15 * * * ?")
//    @Scheduled(cron = "0/10 * * * * ? ") //间隔60秒执行
    // 每隔2小时执行一次；
    // @Scheduled(cron = "0 0 0/2 * * ?")
    public void scheduledUpdateUserNameByToken() {
        execitonScheduledTaskList.doScheduledUpdateUserNameByToken(Const.SCHEDULED_UPDATE_USER_NAME_BY_TOKEN);
    }

    /**
     * <B>方法名称：scheduledUpdateUserNameByGlobalTraceId</B>
     * <B>概要说明：定时基于globalTraceId更新用户名</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 15:08:36
     * @Param []
     **/
    // 每隔30分钟执行一次：
    @Scheduled(cron = "0 */12 * * * ?")
//    @Scheduled(cron = "0/10 * * * * ? ") //间隔60秒执行

    // 每隔2小时执行一次；
    // @Scheduled(cron = "0 0 0/2 * * ?")
    public void scheduledUpdateUserNameByGlobalTraceId() {
        execitonScheduledTaskList.doScheduledUpdateUserNameByGlobalTraceId(Const.SCHEDULED_UPDATE_USER_NAME_BY_GLOBAL_TRACEID);
    }

    /**
     * <B>方法名称：scheduledGetDmsAuditLog</B>
     * <B>概要说明：定时获取dms的审计日志</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年05月26日 17:05:44
     * @Param []
     **/
    /**
     * 每隔30分钟执行一次：
     */
//  @Scheduled(cron = "0 */1 * * * ?")
    // @Scheduled(cron = "0 */30 * * * ?")
    public void scheduledGetDmsAuditLog() {
        execitonScheduledTaskList.doScheduledGetDmsAuditLog(Const.SCHEDULED_GET_DMS_AUDIT_LOG);
    }

    /**
     * <B>方法名称：scheduledUpdateAgentServerHeartBeat</B>
     * <B>概要说明：更新可视化服务端的存活时间到数据库表中，用于生成报告时，获取可视化系统的存活时间</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-05 15:39:42
     * @Param []
     * 每隔60分钟执行一次：
     **/
    @Scheduled(cron = "0 */60 * * * ?")
    public void scheduledUpdateAgentServerHeartBeat() {
        execitonScheduledTaskList.doScheduledUpdateAgentServerHeartBeat(Const.REPORT_AGENT_SERVER_NAME_KEY);
    }

    /**
     * <B>方法名称：doScheduledHandleNoUserName</B>
     * <B>概要说明：将六个小时之前收到的没有登录系统用户名的信息，把数据库用户名当做登录系统的用户名。
     * 这些没有登录系统的用户名，主要分为三类：
     * 1）定时任务，这个已能够实时处理；
     * 2）不需要登录，这个和蓝景的晓军已确认过，确实存在很多不需要登录的接口；
     * 3）探针出现了bug，不能捕获登录系统的用户名，但这种情况基本很少出现。</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-31 09:52:02
     * @Param [key]
     **/
    @Scheduled(cron = "0 */15 * * * ?")
//    @Scheduled(cron = "0/10 * * * * ? ") //间隔60秒执行
    public void scheduledHandleNoUserName() {
        execitonScheduledTaskList.doScheduledHandleNoUserName(Const.SCHEDULED_HANDLE_NO_USER_NAME);
    }

    /**
     * <B>方法名称：scheduledHandleNoUserName</B>
     * <B>概要说明：每间隔5分钟更新下本地内存中的用户来源信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-23 17:28:15
     * @Param []
     **/
    @Scheduled(cron = "0/300 * * * * ? ") //间隔300秒执行
    public void scheduledHandleUserFrom() {
        mingshiServerUtil.setUserFromByDb(msCaffeine.getUserFromCache());
    }

    /**
     * <B>方法名称：scheduledHandleGetUserFromVisitedTimesFromRedis</B>
     * <B>概要说明：从Redis中获取每个用户来源的访问次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-25 09:57:59
     * @Param []
     **/
    @Scheduled(cron = "0/60 * * * * ? ") //间隔60秒执行
    @Async
    public void scheduledHandleGetUserFromVisitedTimesFromRedis() {
        // 从Redis缓存中获取所有的用户；
        Set<String> userNames = redisPoolUtil.smembers(Const.SET_DATA_STATISTICS_HOW_MANY_USERS);
        if (null != userNames && !userNames.isEmpty()) {
            for (String userName : userNames) {
                String key = Const.ZSET_TABLE_BY_EVERYONE_EVERYDAYUSER_FROM_VISITED_TIMES + userName;
                Long aLong = redisPoolUtil.sizeFromZset(key);
                // 从Redis中获取每个用户来源的访问次数；2022-11-25 09:57:55
                Set<ZSetOperations.TypedTuple<String>> typedTuples = redisPoolUtil.reverseRangeWithScores(key, 0L, aLong);
                if (null != typedTuples && !typedTuples.isEmpty()) {
                    // 当Redis中有这个表时，才插入到数据库中。否则会造成数据分布页面数据不准确。2022-11-10 09:03:12
                    for (ZSetOperations.TypedTuple<String> typedTuple : typedTuples) {
                        Double score = typedTuple.getScore();
                        String value = typedTuple.getValue();
                        String[] split = value.split(Const.POUND_KEY);
                        String time = split[0];
                        String userFromName = split[1];
                        msCaffeine.setUserFromVisitedTimesMap(userName, userFromName, time, score.intValue());
                    }
                }
            }
        }
    }
}
