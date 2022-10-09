package com.mingshi.skyflying.task;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.caffeine.MsCaffeine;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsScheduledTaskDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.MsSegmentDetailUsernameIsNullMapper;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

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
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetSegmentDetailDo()# 定时基于token更新用户名。其分布式锁的 key = 【{}】.", key);
    try {
      // 先从 ms_segment_detail_username_is_null 表中获取用户名为空的token；2022-08-01 15:17:21
      List<MsSegmentDetailDo> segmentDetaiDolList = msSegmentDetailUsernameIsNullMapper.selectAllUserNameIsNotNull();
      while (null != segmentDetaiDolList && 0 < segmentDetaiDolList.size()) {
        LinkedList<MsAlarmInformationDo> msAlarmInformationDoList = new LinkedList<>();
        anomalyDetectionBusiness.userVisitedIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
        mingshiServerUtil.flushSegmentDetailToDb(segmentDetaiDolList);
        mingshiServerUtil.flushAbnormalToDb(msAlarmInformationDoList);
        msSegmentDetailUsernameIsNullMapper.deleteByIds(segmentDetaiDolList);
        segmentDetaiDolList.clear();
        segmentDetaiDolList = msSegmentDetailUsernameIsNullMapper.selectAllUserNameIsNotNull();
      }
    } catch (Exception e) {
      log.error("# #scheduledGetDmsAuditLog.scheduledUpdateUserNameByToken()# 定时基于token更新用户名时，出现了异常。#", e);
    }
    log.info("执行完毕 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByToken()# 定时基于token更新用户名。耗时【{}】毫秒。其分布式锁的 key = 【{}】.", DateTimeUtil.getTimeMillis(now), key);
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
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByToken()# 定时基于token更新用户名。其分布式锁的 key = 【{}】.", key);
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
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledUpdateUserNameByGlobalTraceId()# 定时基于globalTraceId更新用户名。其分布式锁的 key = 【{}】.", key);
    try {
      // 先从 ms_segment_detail_username_is_null 表中获取用户名为空的token；2022-08-01 15:17:21
      List<String> tokenList = msSegmentDetailUsernameIsNullMapper.selectAllGlobalTraceIdUserNameIsNull();
      for (String globalTraceId : tokenList) {
        // 先从本地缓存Caffeine中根据token获取用户名；
        String userName = null;
        userName = MsCaffeine.getUserNameByGlobalTraceId(globalTraceId);
        if (StringUtil.isNotBlank(userName)) {
          updateUserNameIsNullByGlobalTraceId(userName, globalTraceId);
        } else {
          // 当本地缓存中不存在时，那么从表 ms_segment_detail 中获取；2022-08-01 15:24:34
          userName = msSegmentDetailDao.selectUserNameByGlobalTraceId(globalTraceId);
          if (StringUtil.isNotBlank(userName)) {
            updateUserNameIsNullByGlobalTraceId(userName, globalTraceId);
            MsCaffeine.putUserNameByGlobalTraceId(globalTraceId, userName);
          }
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
}
