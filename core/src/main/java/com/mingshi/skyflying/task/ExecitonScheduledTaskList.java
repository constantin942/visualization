package com.mingshi.skyflying.task;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.common.caffeine.MsCaffeine;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.common.dao.MsSegmentDetailUsernameIsNullMapper;
import com.mingshi.skyflying.common.dao.MsSystemOperationRecordMapper;
import com.mingshi.skyflying.common.domain.MsScheduledTaskDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.domain.MsSystemOperationRecord;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.service.AuditLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;

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
public class ExecitonScheduledTaskList {
    @Resource
    private AuditLogService auditLogService;
    @Resource
    private MsScheduledTaskDao msScheduledTaskDao;
    @Resource
    private MsSegmentDetailUsernameIsNullMapper msSegmentDetailUsernameIsNullMapper;
    @Resource
    private MsSegmentDetailDao msSegmentDetailDao;
    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private AnomalyDetectionBusiness anomalyDetectionBusiness;
    @Resource
    private MsSystemOperationRecordMapper msSystemOperationRecordMapper;

    /**
     * <B>方法名称：doScheduledUpdateAgentServerHeartBeat</B>
     * <B>概要说明：更新可视化服务端的存活时间到数据库表中，用于生成报告时，获取可视化系统的存活时间</B>
     *
     * @Author zm
     * @Date 2022-12-05 15:30:10
     * @Param [key]
     * @return void
     **/
    public void doScheduledUpdateAgentServerHeartBeat(String key) {
        Instant now = Instant.now();
        log.info("开始执行 #scheduledGetDmsAuditLog.doScheduledUpdateAgentServerHeartBeat()# 定时更新可视化服务端的存活时间到数据库表中，用于生成报告时，获取可视化系统的存活时间信息。其分布式锁的 key = 【{}】.当前线程 = 【{}】", key, Thread.currentThread().getName());
        try {
            MsSystemOperationRecord msSystemOperationRecord = new MsSystemOperationRecord();
            msSystemOperationRecord.setSystemName(Const.REPORT_AGENT_SERVER_NAME);
            msSystemOperationRecord.setGmtModified(new Date());
            msSystemOperationRecordMapper.updateByPrimaryKeySelective(msSystemOperationRecord);
        } catch (Exception e) {
            log.error("# #scheduledGetDmsAuditLog.doScheduledUpdateAgentServerHeartBeat()# 定时更新可视化服务端的存活时间到数据库表中，用于生成报告时，获取可视化系统的存活时间信息时，出现了异常。#", e);
        }
        log.info("执行完毕 #scheduledGetDmsAuditLog.doScheduledUpdateAgentServerHeartBeat()# 定时更新可视化服务端的存活时间到数据库表中，用于生成报告时，获取可视化系统的存活时间信息。耗时【{}】毫秒。其分布式锁的 key = 【{}】.", DateTimeUtil.getTimeMillis(now), key);
    }

    /**
     * <B>方法名称：doScheduledDeleteTwoDaysBeforeSegmentDetailDo</B>
     * <B>概要说明：删除两天前用户的访问信息</B>
     *
     * @Author zm
     * @Date 2022-11-07 14:18:04
     * @Param [key]
     * @return void
     **/
    public void doScheduledDeleteTwoDaysBeforeSegmentDetailDo(String key) {
        Instant now = Instant.now();
        log.info("开始执行 #scheduledGetDmsAuditLog.doScheduledDeleteTwoDaysBeforeSegmentDetailDo()# 定时从 ms_segment_detail_username_is_null 表中删除两天前的用户访问信息。其分布式锁的 key = 【{}】.当前线程 = 【{}】", key, Thread.currentThread().getName());
        try {
            msSegmentDetailDao.deleteTwoDaysBefore();
        } catch (Exception e) {
            log.error("# #scheduledGetDmsAuditLog.doScheduledDeleteTwoDaysBeforeSegmentDetailDo()# 定时从 ms_segment_detail_username_is_null 表中删除两天前的用户访问信息时，出现了异常。#", e);
        }
        log.info("执行完毕 #scheduledGetDmsAuditLog.doScheduledDeleteTwoDaysBeforeSegmentDetailDo()# 定时从 ms_segment_detail_username_is_null 表中删除两天前的用户访问信息。耗时【{}】毫秒。其分布式锁的 key = 【{}】.", DateTimeUtil.getTimeMillis(now), key);
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
    public void doScheduledGetSegmentDetailDo(String key) {
        Instant now = Instant.now();
        log.info("开始执行 #scheduledGetDmsAuditLog.doScheduledGetSegmentDetailDo()# 定时从 ms_segment_detail_username_is_null 表中获取用户名不为空的记录。其分布式锁的 key = 【{}】.当前线程 = 【{}】", key, Thread.currentThread().getName());
        try {
            // 先从 ms_segment_detail_username_is_null 表中获取用户名不为空的记录；2022-10-19 10:39:17
            while (true) {
                List<MsSegmentDetailDo> segmentDetaiDolList = msSegmentDetailUsernameIsNullMapper.selectAllUserNameIsNotNull();
                if(null == segmentDetaiDolList || segmentDetaiDolList.isEmpty()){
                    break;
                }
                anomalyDetectionBusiness.userVisitedIsAbnormal(segmentDetaiDolList);
                msSegmentDetailUsernameIsNullMapper.deleteByIds(segmentDetaiDolList);
                mingshiServerUtil.doEnableReactorModel(null, segmentDetaiDolList, null, null);
            }
        } catch (Exception e) {
            log.error("# #scheduledGetDmsAuditLog.doScheduledGetSegmentDetailDo()# 定时从 ms_segment_detail_username_is_null 表中获取用户名不为空的记录时，出现了异常。#", e);
        }
        log.info("执行完毕 #scheduledGetDmsAuditLog.doScheduledGetSegmentDetailDo()# 定时从 ms_segment_detail_username_is_null 表中获取用户名不为空的记录。耗时【{}】毫秒。其分布式锁的 key = 【{}】.", DateTimeUtil.getTimeMillis(now), key);
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
    public void doScheduledUpdateUserNameByToken(String key) {
        Instant now = Instant.now();
        log.info("开始执行 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByToken()# 定时基于token更新用户名。其分布式锁的 key = 【{}】.当前线程 = 【{}】。", key, Thread.currentThread().getName());
        try {
            // 先从 ms_segment_detail_username_is_null 表中获取用户名为空的token；2022-08-01 15:17:21
            List<String> tokenList = msSegmentDetailUsernameIsNullMapper.selectAllTokenUserNameIsNull();
            for (String token : tokenList) {
                // 先从本地缓存Caffeine中根据token获取用户名；
                String userName = null;
                userName = MsCaffeine.getUserNameByToken(token);
                if (StringUtil.isNotBlank(userName)) {
                    updateUserNameIsNullByToken(userName, token);
                } else {
                    // 当本地缓存中不存在时，那么从表 ms_segment_detail 中获取；2022-08-01 15:24:34
                    userName = msSegmentDetailDao.selectUserNameByToken(token);
                    if (StringUtil.isNotBlank(userName)) {
                        updateUserNameIsNullByToken(userName, token);
                        MsCaffeine.putUserNameByToken(token, userName);
                    }
                }
            }
        } catch (Exception e) {
            log.error("# #scheduledGetDmsAuditLog.scheduledUpdateUserNameByToken()# 定时基于token更新用户名时，出现了异常。#", e);
        }
        log.info("执行完毕 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByToken()# 定时基于token更新用户名。耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
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
    public void doScheduledUpdateUserNameByGlobalTraceId(String key) {
        Instant now = Instant.now();
        log.info("开始执行 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByGlobalTraceId()# 定时基于globalTraceId更新用户名。其分布式锁的 key = 【{}】.当前线程 = 【{}】。", key, Thread.currentThread().getName());
        try {
            List<Map<String, String>> mapList = msSegmentDetailUsernameIsNullMapper.selectAllGlobalTraceIdUserNameIsNotNull();
            if (null != mapList && !mapList.isEmpty()) {
                msSegmentDetailUsernameIsNullMapper.updateBatch(mapList);
            }
            for (int i = 0; i < mapList.size(); i++) {
                Map<String, String> stringStringMap = mapList.get(i);
                String globalTraceId = stringStringMap.get(Const.GLOBAL_TRACE_ID);
                String userNameFromDb = stringStringMap.get(Const.USER_NAME);
                String userName = MsCaffeine.getUserNameByGlobalTraceId(globalTraceId);
                if (StringUtil.isBlank(userName)) {
                    // 放入本地缓存；2022-10-13 13:44:26
                    MsCaffeine.putUserNameByGlobalTraceId(globalTraceId, userNameFromDb);
                }
            }
        } catch (Exception e) {
            log.error("# #scheduledGetDmsAuditLog.scheduledUpdateUserNameByGlobalTraceId()# 定时基于 globalTraceId 更新用户名时，出现了异常。#", e);
        }
        log.info("执行完毕 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByGlobalTraceId()# 定时基于 globalTraceId 更新用户名。耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
    }

    /**
     * <B>方法名称：updateUserNameIsNullByToken</B>
     * <B>概要说明：根据token更新用户名为空的记录</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 15:08:12
     * @Param [userNameByToken, token]
     **/
    private void updateUserNameIsNullByToken(String userNameByToken, String token) {
        try {
            HashMap<String, String> map = new HashMap<>(Const.NUMBER_EIGHT);
            map.put("userName", userNameByToken);
            map.put("token", token);
            msSegmentDetailUsernameIsNullMapper.updateUserNameByToken(map);
        } catch (Exception e) {
            log.error("# scheduledGetDmsAuditLog.updateUserNameIsNull() # 根据token更新用户名到表 ms_segment_detail_username_is_null 时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：updateUserNameIsNullByGlobalTraceId</B>
     * <B>概要说明：根据globalTraceId更新用户名为空的记录</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 15:08:12
     * @Param [userNameByToken, token]
     **/
    private void updateUserNameIsNullByGlobalTraceId(String userName, String globalTraceId) {
        try {
            HashMap<String, String> map = new HashMap<>(Const.NUMBER_EIGHT);
            map.put("userName", userName);
            map.put("globalTraceId", globalTraceId);
            msSegmentDetailUsernameIsNullMapper.updateUserNameByGlobalTraceId(map);
        } catch (Exception e) {
            log.error("# scheduledGetDmsAuditLog.updateUserNameIsNull() # 根据globalTraceId更新用户名到表 ms_segment_detail_username_is_null 时，出现了异常。", e);
        }
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
    public void doScheduledGetDmsAuditLog(String key) {
        log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。其分布式锁的 key = 【{}】.", key);
        Instant start = Instant.now();
        String startTime = null;
        String endTime = null;
        MsScheduledTaskDo msScheduledTaskDo = msScheduledTaskDao.selectLastSuccessRecord(Const.RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT);
        if (null == msScheduledTaskDo || StringUtil.isBlank(msScheduledTaskDo.getStartTime())) {
            // 如果定时任务表里不存在操作记录，那么就设置一个默认值；2022-05-26 17:37:27
            startTime = Const.INIT_TIME;
        } else {
            // 获取上一次成功执行完毕的操作事件；2022-05-26 17:37:56
            startTime = msScheduledTaskDo.getEndTime();
        }

        endTime = DateTimeUtil.dateToStrformat(new Date());
        log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。开始时间 startTime = 【{}】，endTime = 【{}】。", startTime, endTime);
        auditLogService.autoFetchAuditlogByDms(startTime, endTime);
        log.info("执行结束 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。耗时 = 【{}】毫秒。", DateTimeUtil.getTimeMillis(start));
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
    public void doScheduledHandleNoUserName(String key) {
        try {
            log.info("开始执行 #scheduledGetDmsAuditLog.doScheduledHandleNoUserName()# 定时将六个小时之前收到的没有登录系统用户名的信息，把数据库用户名当做登录系统的用户名。。其分布式锁的 key = 【{}】.", key);
            Instant start = Instant.now();
            String startTime = null;
            String endTime = null;

            Instant minus = Instant.now().minus(Const.NUMBER_SIX, ChronoUnit.HOURS);

            String queryTime = DateTimeUtil.instantToString1(minus);
            // 获取六小时之前没有用户名的记录；2022-10-31 10:06:13
            List<Map<String, String>> list = msSegmentDetailUsernameIsNullMapper.selectAllNoUserNameBeforeSixHours(queryTime);
            if(null != list && !list.isEmpty()){
                for (int i = 0; i < list.size(); i++) {
                    Map<String, String> map = list.get(i);
                    map.put(Const.USER_FROM,Const.USER_FROM_SCHEDULE_TASK);
                }
                msSegmentDetailUsernameIsNullMapper.updateNoUserName(list);
            }

            endTime = DateTimeUtil.dateToStrformat(new Date());
            log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。开始时间 startTime = 【{}】，endTime = 【{}】。", startTime, endTime);
            auditLogService.autoFetchAuditlogByDms(startTime, endTime);
            log.info("执行结束 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。耗时 = 【{}】毫秒。", DateTimeUtil.getTimeMillis(start));
        } catch (Exception e) {
            log.error("# scheduledGetDmsAuditLog.doScheduledHandleNoUserName() # 将六个小时之前收到的没有登录系统用户名的信息，把数据库用户名当做登录系统的用户名时，出现了异常。", e);
        }
    }
}
