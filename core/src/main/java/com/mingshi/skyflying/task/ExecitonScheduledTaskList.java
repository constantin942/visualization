package com.mingshi.skyflying.task;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionUtil;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.caffeine.MsCaffeine;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.enums.ConstantsCode;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.init.rule.LoadUserPortraitFromDb;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

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
  private LoadUserPortraitFromDb loadUserPortraitFromDb;
  @Resource
  private MingshiServerUtil mingshiServerUtil;
  @Resource
  private UserPortraitByVisitedTimeMapper userPortraitByVisitedTimeMapper;
  @Resource
  private UserPortraitByVisitedTableEverydayMapper userPortraitByVisitedTableEverydayMapper;

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
      List<MsSegmentDetailDo> tokenList = msSegmentDetailUsernameIsNullMapper.selectAllUserNameIsNotNull();
      while (null != tokenList && 0 < tokenList.size()) {
        // 将从表 ms_segment_detail_username_is_null 获取到用户名不为空的记录，放入到公共队列中，让其IoThread线程进行持久化操作（在Redis中统计以及保存到MySQL中）；2022-08-01 15:50:47
        mingshiServerUtil.doEnableReactorModel(null, null, null, tokenList, null, null, null);
        // 从表 ms_segment_detail_username_is_null 将用户名不为空的记录删除。这里有个丢数据的场景：当数据放入到公共队列后 ，此时当前服务宕机了，那么在队列里还没来得及进行持久化的数据就丢失了。2022-08-01 15:59:12
        msSegmentDetailUsernameIsNullMapper.deleteByIds(tokenList);
        tokenList = msSegmentDetailUsernameIsNullMapper.selectAllUserNameIsNotNull();
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
   * <B>方法名称：scheduledUpdateUserPortraitByVisitedTime</B>
   * <B>概要说明：定时更新基于访问时间的用户画像信息</B>
   * 只有用户画像信息有变更了，才将其更新到数据库中。其目的是减少数据库的访问压力。
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 10:06:01
   * @Param []
   **/
  public void doScheduledUpdateUserPortrait(String key) {
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledUpdateUserPortrait()#定时更新基于访问时间的用户画像信息。其分布式锁的 key = 【{}】.", key);

    // 将基于用户访问时间的画像信息更新到数据库中
    updateUserPortraitByVisitedTime();
    // 将基于用户访问过的表的画像信息更新到数据库中
    updateUserPortraitByVisitedTable();

  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTable</B>
   * <B>概要说明：将基于用户访问过的表的画像信息更新到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月09日 09:06:18
   * @Param []
   **/
  private void updateUserPortraitByVisitedTable() {
    try {
      Instant start = Instant.now();
      Boolean isChangedAtomicBoolean = AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableIsChanged();
      if (false == isChangedAtomicBoolean) {
        log.info("# scheduledGetDmsAuditLog.updateUserPortraitByVisitedTable() # 基于访问过的表的用户画像统计信息没有变更，此次定时任务就不更新到数据库中了。");
        return;
      }

      log.info("# scheduledGetDmsAuditLog.updateUserPortraitByVisitedTable() # 基于访问过的表的用户画像统计信息有变更，此次定时任务将其更新到数据库中了。");
      Boolean userPortraitByVisitedTableEnable = AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableEnable();
      if (false == userPortraitByVisitedTableEnable) {
        return;
      }
      Map<String/* 用户名 */,
        Map<String/* 访问过的表 */,
          Map<String/* 访问日期，以天为单位 */,
            Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>> userVisitedTableDateCountMap =
        AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableMap();
      if (null == userVisitedTableDateCountMap || 0 == userVisitedTableDateCountMap.size()) {
        // 如果数据库中不存在该画像信息，那么每次都要去数据库中获取一次，会给数据库造成很大的压力。所以这里先注释掉。2022-06-09 08:29:33
        // loadUserPortraitFromDb.initUserPortraitByVisitedTableMap();
        return;
      }

      List<UserPortraitByVisitedTableEverydayDo> list = new LinkedList<>();

      Map<String/* 用户名 */,
        Map<String/* 访问过的表 */,
          Map<String/* 访问日期，以天为单位 */,
            Map<String,/* 数据库操作类型：insert、delete、update、select */
              Integer/* 访问次数 */>>>> newMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
      newMap.putAll(userVisitedTableDateCountMap);
      Iterator<String> iterator = newMap.keySet().iterator();
      while (iterator.hasNext()) {
        String userName = iterator.next();
        Map<String/* 访问过的表 */,
          Map<String/* 访问日期，以天为单位 */,
            Map<String,/* 数据库操作类型：insert、delete、update、select */
              Integer/* 访问次数 */>>> visitedTableDateCountMap = newMap.get(userName);
        Iterator<String> iterator2 = visitedTableDateCountMap.keySet().iterator();
        while (iterator2.hasNext()) {
          String tableName = iterator2.next();
          Map<String/* 访问日期，以天为单位 */,
            Map<String,/* 数据库操作类型：insert、delete、update、select */
              Integer/* 访问次数 */>> dateDbTypeCountMap = visitedTableDateCountMap.get(tableName);
          if (null != dateDbTypeCountMap) {
            Iterator<String> iterator1 = dateDbTypeCountMap.keySet().iterator();
            while (iterator1.hasNext()) {
              String dateTime = iterator1.next();
              Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */> dbTypeCountMap = dateDbTypeCountMap.get(dateTime);
              Iterator<String> iterator3 = dbTypeCountMap.keySet().iterator();
              while (iterator3.hasNext()) {
                String dbType = iterator3.next();
                Integer visitedCount = dbTypeCountMap.get(dbType);
                UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableDo = new UserPortraitByVisitedTableEverydayDo();
                userPortraitByVisitedTableDo.setVisitedDate(dateTime);
                userPortraitByVisitedTableDo.setUserName(userName);
                userPortraitByVisitedTableDo.setVisitedTable(tableName);
                userPortraitByVisitedTableDo.setVisitedCount(visitedCount);
                userPortraitByVisitedTableDo.setDbType(dbType);
                list.add(userPortraitByVisitedTableDo);
              }
            }
          }
        }
      }

      if (!list.isEmpty()) {
        try {
          userPortraitByVisitedTableEverydayMapper.insertSelectiveBatch(list);
          log.info("# scheduledGetDmsAuditLog.scheduledUpdateUserPortraitByVisitedTime() # 定时更新基于访问时间的用户画像信息【{}条】标识耗时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(start));
          AnomylyDetectionSingletonByVisitedTableEveryday.setUserPortraitByVisitedTableIsChanged(false);
        } catch (Exception e) {
          log.error("# scheduledGetDmsAuditLog.scheduledUpdateUserPortraitByVisitedTime() # 定时更新基于访问时间的用户画像信息时，出现了异常。", e);
        }
      }
    } catch (Exception e) {
      log.error("# ScheduledTask.updateUserPortraitByVisitedTable() # 将基于用户访问过的表的画像信息更新到数据库中时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTime</B>
   * <B>概要说明：将基于用户访问时间的画像信息更新到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月09日 09:06:01
   * @Param []
   **/
  private void updateUserPortraitByVisitedTime() {
    try {
      Instant start = Instant.now();
      Boolean isChangedAtomicBoolean = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeIsChanged();
      if (false == isChangedAtomicBoolean) {
        log.info("# scheduledGetDmsAuditLog.scheduledUpdateUserPortraitByVisitedTime() # 基于访问时间的用户画像统计信息没有变更，此次定时任务就不更新到数据库中了。");
        return;
      }

      log.info("# scheduledGetDmsAuditLog.scheduledUpdateUserPortraitByVisitedTime() # 基于访问时间的用户画像统计信息有变更，此次定时任务将其更新到数据库中了。");
      Boolean userPortraitByVisitedTimeEnable = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeEnable();
      if (false == userPortraitByVisitedTimeEnable) {
        // 这条规则没有启用，那么就直接返回；2022-06-23 16:09:28
        return;
      }
      Map<String, Map<String, Integer>> oldMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
      if (null == oldMap || 0 == oldMap.size()) {
        // 如果数据库中不存在该画像信息，那么每次都要去数据库中获取一次，会给数据库造成很大的压力。所以这里先注释掉。2022-06-09 08:29:33
        // loadUserPortraitFromDb.initUserPortraitByVisitedTimeMap();
        return;
      }

      List<UserPortraitByVisitedTimeDo> list = new LinkedList<>();
      ConcurrentHashMap<String, Map<String, Integer>> newMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
      newMap.putAll(oldMap);
      Iterator<String> iterator = newMap.keySet().iterator();
      while (iterator.hasNext()) {
        UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo = new UserPortraitByVisitedTimeDo();
        list.add(userPortraitByVisitedTimeDo);
        String userName = iterator.next();
        userPortraitByVisitedTimeDo.setUserName(userName);

        Map<String, Integer> map = newMap.get(userName);
        if (null != map) {
          Integer forenoonCount = map.get(ConstantsCode.USER_PORTRAIT_FORENOON.getCode());
          if (null != forenoonCount) {
            userPortraitByVisitedTimeDo.setForenoonCount(forenoonCount);
          }
          Integer afternoonCount = map.get(ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode());
          if (null != afternoonCount) {
            userPortraitByVisitedTimeDo.setAfternoonCount(afternoonCount);
          }
          Integer nightCount = map.get(ConstantsCode.USER_PORTRAIT_NIGHT.getCode());
          if (null != nightCount) {
            userPortraitByVisitedTimeDo.setNightCount(nightCount);
          }
        }
      }

      if (!list.isEmpty()) {
        try {
          userPortraitByVisitedTimeMapper.updateBatch(list);
          log.info("# scheduledGetDmsAuditLog.scheduledUpdateUserPortraitByVisitedTime() # 定时更新基于访问时间的用户画像信息【{}条】标识耗时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(start));
          AnomylyDetectionSingletonByVisitedTime.setUserPortraitByVisitedTimeIsChanged(false);
        } catch (Exception e) {
          log.error("# scheduledGetDmsAuditLog.scheduledUpdateUserPortraitByVisitedTime() # 定时更新基于访问时间的用户画像信息时，出现了异常。", e);
        }
      }
    } catch (Exception e) {
      log.error("# ScheduledTask.updateUserPortraitByVisitedTime() # 将基于用户访问时间的画像信息更新到数据库中时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：scheduledGetNoCheckAbnormalRecord</B>
   * <B>概要说明：定时将数据库中用户名不为空，且未进行过基于访问时间的异常检测记录查询出来，然后进行异常检测</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 09:06:58
   * @Param []
   **/
  public void doScheduledGetNoCheckAbnormalRecord(String key) {
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetNoCheckAbnormalRecord()# 定时将数据库中用户名不为空，且未进行过基于访问时间的异常检测记录查询出来，然后进行异常检测。其分布式锁的 key = 【{}】.", key);
    Instant start = Instant.now();

    LinkedList<MsAlarmInformationDo> msAlarmInformationDoLinkedListist = new LinkedList<>();
    List<MsSegmentDetailDo> userNameIsNotNullAndVisitedTimeList = new LinkedList<>();
    // 将基于访问时间的还没有进行检测的记录进行检测
    getNoCheckVisitedTimeAbnormalRecord(msAlarmInformationDoLinkedListist, userNameIsNotNullAndVisitedTimeList);

    // 将基于访问过的表还没有进行检测的记录进行检测
    getNoCheckVisitedTableAbnormalRecord(msAlarmInformationDoLinkedListist, userNameIsNotNullAndVisitedTimeList);

    // 将异常信息批量插入到MySQL中；2022-06-07 18:16:44
    mingshiServerUtil.flushAbnormalToDb(msAlarmInformationDoLinkedListist);
    log.info("执行结束 #scheduledGetDmsAuditLog.scheduledGetNoCheckAbnormalRecord()# 定时将数据库中用户名不为空，且未进行过基于访问时间的异常检测记录查询出来，然后进行异常检测。耗时 = 【{}】毫秒。", DateTimeUtil.getTimeMillis(start));

    try {
      if (!userNameIsNotNullAndVisitedTimeList.isEmpty()) {
        Instant now = Instant.now();
        msSegmentDetailDao.updateBatchById(userNameIsNotNullAndVisitedTimeList);
        log.info("# scheduledGetDmsAuditLog.scheduledGetNoCheckAbnormalRecord() # 定时任务批量更新基于访问时间的异常信息【{}条】标识耗时 = 【{}】毫秒。", userNameIsNotNullAndVisitedTimeList.size(), DateTimeUtil.getTimeMillis(now));
        AnomylyDetectionSingletonByVisitedTime.setUserPortraitByVisitedTimeIsChanged(true);
      }
    } catch (Exception e) {
      log.error("# scheduledGetDmsAuditLog.scheduledGetNoCheckAbnormalRecord() # 定时任务批量更新基于访问时间的异常信息标识出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getNoCheckVisitedTableAbnormalRecord</B>
   * <B>概要说明：将基于访问过的表还没有进行检测的记录进行检测</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月09日 09:06:42
   * @Param [msAlarmInformationDoLinkedListist, userNameIsNotNullAndVisitedTimeList]
   **/
  private void getNoCheckVisitedTableAbnormalRecord(LinkedList<MsAlarmInformationDo> msAlarmInformationDoLinkedListist, List<MsSegmentDetailDo> userNameIsNotNullAndVisitedTimeList) {
    try {
      List<MsSegmentDetailDo> userNameIsNotNullAndVisitedTableList = msSegmentDetailDao.selectAllUserNameIsNotNullAndVisitedTableIsZero();
      if (null != userNameIsNotNullAndVisitedTableList && !userNameIsNotNullAndVisitedTableList.isEmpty()) {
        userNameIsNotNullAndVisitedTimeList.addAll(userNameIsNotNullAndVisitedTableList);
        AnomalyDetectionUtil.userVisitedTableIsAbnormal(userNameIsNotNullAndVisitedTableList, msAlarmInformationDoLinkedListist);
      }
    } catch (Exception e) {
      log.error("# ScheduledTask.getNoCheckVisitedTableAbnormalRecord() # 将基于访问过的表还没有进行检测的记录进行检测时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getNoCheckVisitedTimeAbnormalRecord</B>
   * <B>概要说明：将基于访问时间的还没有进行检测的记录进行检测</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月09日 09:06:42
   * @Param [msAlarmInformationDoLinkedListist, userNameIsNotNullAndVisitedTimeList]
   **/
  private void getNoCheckVisitedTimeAbnormalRecord(LinkedList<MsAlarmInformationDo> msAlarmInformationDoLinkedListist, List<MsSegmentDetailDo> userNameIsNotNullAndVisitedTimeList) {
    Boolean userPortraitByVisitedTimeEnable = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeEnable();
    if (false == userPortraitByVisitedTimeEnable) {
      // 这条规则没有启用，那么就直接返回；2022-06-23 16:09:28
      return;
    }
    try {
      List<MsSegmentDetailDo> msSegmentDetailDoList = msSegmentDetailDao.selectAllUserNameIsNotNullAndVisitedTimeIsZero();
      if (null != msSegmentDetailDoList && !msSegmentDetailDoList.isEmpty()) {
        userNameIsNotNullAndVisitedTimeList.addAll(msSegmentDetailDoList);
        // 如果用户画像还没有加载到本地内存中来，那么先加载进来，然后在进行异常检测；
        Map<String, Map<String, Integer>> oldUserPortraitByVisitedTimeMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
        if (null == oldUserPortraitByVisitedTimeMap || 0 == oldUserPortraitByVisitedTimeMap.size()) {
          loadUserPortraitFromDb.getAllUserPortraitRules();
          return;
        }

        for (MsSegmentDetailDo msSegmentDetailDo : userNameIsNotNullAndVisitedTimeList) {
          msSegmentDetailDo.setUserPortraitFlagByVisitedTime(1);
          String userName = String.valueOf(msSegmentDetailDo.getUserName());
          String startTime = String.valueOf(msSegmentDetailDo.getStartTime());
          String globalTraceId = String.valueOf(msSegmentDetailDo.getGlobalTraceId());
          SegmentDo segmentDo = new SegmentDo();
          segmentDo.setUserName(userName);
          segmentDo.setRequestStartTime(startTime);
          segmentDo.setGlobalTraceId(globalTraceId);

          AnomalyDetectionUtil.userVisitedTimeIsAbnormal(segmentDo, msAlarmInformationDoLinkedListist);
        }
      }
    } catch (Exception e) {
      log.error("# ScheduledTask.getNoCheckVisitedTimeAbnormalRecord() # 将基于访问过的表还没有进行检测的记录进行检测时，出现了异常。", e);
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