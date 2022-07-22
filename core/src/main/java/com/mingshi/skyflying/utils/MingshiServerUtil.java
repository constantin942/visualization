package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.agent.AgentInformationSingleton;
import com.mingshi.skyflying.config.SingletonLocalStatisticsMap;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.elasticsearch.domain.EsMsSegmentDetailDo;
import com.mingshi.skyflying.elasticsearch.utils.MingshiElasticSearchUtil;
import com.mingshi.skyflying.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.statistics.InformationOverviewSingleton;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.utils.CopyOnWriteMap;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: mingshiServerUtil</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/5/30 20:46
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class MingshiServerUtil {
  @Resource
  private RedisPoolUtil redisPoolUtil;
  @Resource
  private MingshiElasticSearchUtil mingshiElasticSearchUtil;
  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private MsAlarmInformationMapper msAlarmInformationMapper;
  @Resource
  private MsAgentInformationMapper msAgentInformationMapper;
  @Resource
  private SpanMapper spanMapper;
  @Resource
  private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
  @Resource
  private MsAuditLogDao msAuditLogDao;
  @Resource
  private SegmentDao segmentDao;
  @Resource
  private UserTokenDao userTokenDao;

  /**
   * <B>方法名称：getSqlType</B>
   * <B>概要说明：获取SQL语句的类型</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年05月28日 12:05:33
   * @Param [msSql]
   **/
  public String getSqlType(String msSql) {
    if (msSql.startsWith(Const.SQL_TYPE_SELECT) || msSql.startsWith(Const.SQL_TYPE_SELECT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SELECT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SELECT)) {
      return Const.SQL_TYPE_SELECT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_INSERT) || msSql.startsWith(Const.SQL_TYPE_INSERT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_INSERT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_INSERT)) {
      return Const.SQL_TYPE_INSERT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_UPDATE) || msSql.startsWith(Const.SQL_TYPE_UPDATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_UPDATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_UPDATE)) {
      return Const.SQL_TYPE_UPDATE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DELETE) || msSql.startsWith(Const.SQL_TYPE_DELETE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DELETE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DELETE)) {
      return Const.SQL_TYPE_DELETE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_LOGIN) || msSql.startsWith(Const.SQL_TYPE_LOGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGIN)) {
      return Const.SQL_TYPE_LOGIN.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_LOGOUT) || msSql.startsWith(Const.SQL_TYPE_LOGOUT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGOUT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGOUT)) {
      return Const.SQL_TYPE_LOGOUT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_MERGE) || msSql.startsWith(Const.SQL_TYPE_MERGE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_MERGE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_MERGE)) {
      return Const.SQL_TYPE_MERGE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_ALTER) || msSql.startsWith(Const.SQL_TYPE_ALTER.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ALTER.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ALTER)) {
      return Const.SQL_TYPE_ALTER.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_CREATEINDEX) || msSql.startsWith(Const.SQL_TYPE_CREATEINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATEINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATEINDEX)) {
      return Const.SQL_TYPE_CREATEINDEX.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DROPINDEX) || msSql.startsWith(Const.SQL_TYPE_DROPINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROPINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROPINDEX)) {
      return Const.SQL_TYPE_DROPINDEX.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_CREATE) || msSql.startsWith(Const.SQL_TYPE_CREATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATE)) {
      return Const.SQL_TYPE_CREATE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DROP) || msSql.startsWith(Const.SQL_TYPE_DROP.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROP.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROP)) {
      return Const.SQL_TYPE_DROP.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_SET) || msSql.startsWith(Const.SQL_TYPE_SET.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SET.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SET)) {
      return Const.SQL_TYPE_SET.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DESC) || msSql.startsWith(Const.SQL_TYPE_DESC.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESC.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESC)) {
      return Const.SQL_TYPE_DESC.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_REPLACE) || msSql.startsWith(Const.SQL_TYPE_REPLACE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_REPLACE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_REPLACE)) {
      return Const.SQL_TYPE_REPLACE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_CALL) || msSql.startsWith(Const.SQL_TYPE_CALL.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CALL.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CALL)) {
      return Const.SQL_TYPE_CALL.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_BEGIN) || msSql.startsWith(Const.SQL_TYPE_BEGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_BEGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_BEGIN)) {
      return Const.SQL_TYPE_BEGIN.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DESCRIBE) || msSql.startsWith(Const.SQL_TYPE_DESCRIBE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESCRIBE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESCRIBE)) {
      return Const.SQL_TYPE_DESCRIBE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_ROLLBACK) || msSql.startsWith(Const.SQL_TYPE_ROLLBACK.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ROLLBACK.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ROLLBACK)) {
      return Const.SQL_TYPE_ROLLBACK.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_FLUSH) || msSql.startsWith(Const.SQL_TYPE_FLUSH.toLowerCase()) || msSql.contains(Const.SQL_TYPE_FLUSH.toLowerCase()) || msSql.contains(Const.SQL_TYPE_FLUSH)) {
      return Const.SQL_TYPE_FLUSH.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_USE) || msSql.startsWith(Const.SQL_TYPE_USE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_USE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_USE)) {
      return Const.SQL_TYPE_USE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_SHOW) || msSql.startsWith(Const.SQL_TYPE_SHOW.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SHOW.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SHOW)) {
      return Const.SQL_TYPE_SHOW.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_START) || msSql.startsWith(Const.SQL_TYPE_START.toLowerCase()) || msSql.contains(Const.SQL_TYPE_START.toLowerCase()) || msSql.contains(Const.SQL_TYPE_START)) {
      return Const.SQL_TYPE_START.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_COMMIT) || msSql.startsWith(Const.SQL_TYPE_COMMIT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_COMMIT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_COMMIT)) {
      return Const.SQL_TYPE_COMMIT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_RENAME) || msSql.startsWith(Const.SQL_TYPE_RENAME.toLowerCase()) || msSql.contains(Const.SQL_TYPE_RENAME.toLowerCase()) || msSql.contains(Const.SQL_TYPE_RENAME)) {
      return Const.SQL_TYPE_RENAME.toLowerCase();
    } else if (msSql.equals("keys *")) {
      return null;
    }
    log.error("#SegmentConsumeServiceImpl.getSqlType() #没有匹配到SQL的类型，这是不正常的。需要好好的排查下，当前SQL = 【{}】。", msSql);
    return null;
  }

  /**
   * <B>方法名称：getTableName</B>
   * <B>概要说明：根据SQL类型和SQL语句，获取表名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年06月06日 14:06:39
   * @Param [sqlType, msSql]
   **/
  public String getTableName(String sqlType, String msSql) {
    String tableName = null;
    if (StringUtil.isBlank(sqlType)) {
      return tableName;
    }
    List<String> tableNameList = null;
    if (sqlType.equals(Const.SQL_TYPE_SELECT.toLowerCase())) {
      tableNameList = SqlParserUtils.selectTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_INSERT.toLowerCase())) {
      tableNameList = SqlParserUtils.insertTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_UPDATE.toLowerCase())) {
      tableNameList = SqlParserUtils.updateTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_DELETE.toLowerCase())) {
      tableNameList = SqlParserUtils.deleteTable(msSql);
    } else {
      log.error("# SegmentConsumeServiceImpl.getMsAuditLogDo() # 根据SQL语句 = 【{}】获取表名时，该SQL语句不是select、insert、update、delete。", msSql);
    }
    if (null != tableNameList && 0 < tableNameList.size()) {
      for (String table : tableNameList) {
        if (StringUtil.isBlank(tableName)) {
          tableName = table;
        } else {
          tableName = tableName + "," + table;
        }
      }
    }
    return tableName;
  }

  public List<String> getTableNameList(String sqlType, String msSql) {
    List<String> tableNameList = null;
    if (StringUtil.isBlank(sqlType)) {
      return tableNameList;
    }
    if (sqlType.equals(Const.SQL_TYPE_SELECT.toLowerCase())) {
      tableNameList = SqlParserUtils.selectTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_INSERT.toLowerCase())) {
      tableNameList = SqlParserUtils.insertTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_UPDATE.toLowerCase())) {
      tableNameList = SqlParserUtils.updateTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_DELETE.toLowerCase())) {
      tableNameList = SqlParserUtils.deleteTable(msSql);
    } else {
      // log.error("# SegmentConsumeServiceImpl.getMsAuditLogDo() # 根据SQL语句 = 【{}】获取表名时，该SQL语句不是select、insert、update、delete。", msSql);
    }
    return tableNameList;
  }

  /**
   * <B>方法名称：updateUserNameByGlobalTraceId</B>
   * <B>概要说明：根据全局追踪id更新用户名</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月24日 11:05:07
   * @Param [now]
   **/
  public void updateUserNameByGlobalTraceId() {
    Boolean atomicBoolean = SingletonLocalStatisticsMap.getAtomicBooleanIsChanged();
    if (false == atomicBoolean) {
      // 只有当索引有变动的时候，才把数据更新到数据库中；2022-05-24 17:15:55
      return;
    }

    Boolean booleanIsUpdatingData = SingletonLocalStatisticsMap.getAtomicBooleanIsUpdatingData();
    if (true == booleanIsUpdatingData) {
      // 只有其他线程没有执行刷新操作时，本线程才执行；2022-05-24 17:15:55
      return;
    }
    // 这里应该加个正在执行更新的标志；考虑这样一种场景：在同一个jvm进程内，有多个 IoThread 线程在执行，在同一时间应该只有一个 IoThread 线程执行更新操作。
    // 2022-05-24 17:17:55
    Map<String/* token */, String/* userName */> tokenUserNameMap = SingletonLocalStatisticsMap.getTokenAndUserNameMap();
    Map<String/* globalTraceId */, String/* userName */> globalTraceIdAndUserNameMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndUserNameMap();
    Map<String/* globalTraceId */, String/* token */> globalTraceIdTokenMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndTokenMapMap();
    Iterator<String> iterator = globalTraceIdAndUserNameMap.keySet().iterator();
    // List<UserTokenDo> userTokenDoList = new LinkedList<>();
    // List<MsAuditLogDo> auditLogDoList = new LinkedList<>();
    List<MsSegmentDetailDo> setmentDetailDoList = new LinkedList<>();
    while (iterator.hasNext()) {
      String globalTraceId = iterator.next();
      String userName = globalTraceIdAndUserNameMap.get(globalTraceId);
      String token = globalTraceIdTokenMap.get(globalTraceId);
      if (StringUtil.isBlank(userName)) {
        userName = tokenUserNameMap.get(token);
      }
      if (StringUtil.isBlank(userName)) {
        log.error("# IoThread.updateUserNameByGlobalTraceId # 将索引插入到数据库中的时候，出现了异常。userName = null，globalTraceId = 【{}】，token = 【{}】。", globalTraceId, token);
        continue;
      }

      // UserTokenDo userTokenDo = new UserTokenDo();
      // userTokenDo.setUserName(userName);
      // userTokenDo.setGlobalTraceId(globalTraceId);
      // userTokenDo.setToken(token);
      // userTokenDoList.add(userTokenDo);

      MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
      msSegmentDetailDo.setGlobalTraceId(globalTraceId);
      msSegmentDetailDo.setUserName(userName);
      setmentDetailDoList.add(msSegmentDetailDo);

      // MsAuditLogDo msAuditLogDo = new MsAuditLogDo();
      // msAuditLogDo.setGlobalTraceId(globalTraceId);
      // msAuditLogDo.setApplicationUserName(userName);
      // auditLogDoList.add(msAuditLogDo);
    }
    // 批量插入用户名、token、global信息
    // batchInsertUserToken(userTokenDoList);

    // 批量更新审计日志的用户名和globalTraceId信息；
    // batchUpdateMsAuditLog(auditLogDoList);

    // 批量更新segmentDetail信息的用户名和globalTraceId信息；
    batchUpdateMsSegmentDetail(setmentDetailDoList);

    SingletonLocalStatisticsMap.setAtomicBooleanIsUpdatingData(false);
    SingletonLocalStatisticsMap.setAtomicBooleanIsChanged(false);
  }

  /**
   * <B>方法名称：batchUpdateMsAuditLog</B>
   * <B>概要说明：批量更新审计日志的用户名和globalTraceId信息；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 11:06:48
   * @Param [auditLogDoList]
   **/
  private void batchUpdateMsSegmentDetail(List<MsSegmentDetailDo> setmentDetailDoList) {
    try {
      if (0 < setmentDetailDoList.size()) {
        Instant now = Instant.now();
        msSegmentDetailDao.updateBatch(setmentDetailDoList);
        log.info("# IoThread.batchUpdateMsSegmentDetail # 更新数据库审计数据（【{}】条）的用户名耗时【{}】毫秒。", setmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# IoThread.batchUpdateMsSegmentDetail # 批量更新审计日志中的登录应用系统的用户名时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：batchUpdateMsAuditLog</B>
   * <B>概要说明：批量更新审计日志的用户名和globalTraceId信息；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 11:06:48
   * @Param [auditLogDoList]
   **/
  private void batchUpdateMsAuditLog(List<MsAuditLogDo> auditLogDoList) {
    try {
      if (0 < auditLogDoList.size()) {
        Instant now = Instant.now();
        msAuditLogDao.updateBatch(auditLogDoList);
        log.info("# IoThread.batchUpdateMsAuditLog # 更新数据库审计数据（【{}】条）的用户名耗时【{}】毫秒。", auditLogDoList.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# IoThread.batchUpdateMsAuditLog # 批量更新审计日志中的登录应用系统的用户名时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：batchInsertUserToken</B>
   * <B>概要说明：批量插入用户名、token、global信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 11:06:39
   * @Param [userTokenDoList]
   **/
  private void batchInsertUserToken(List<UserTokenDo> userTokenDoList) {
    try {
      if (0 < userTokenDoList.size()) {
        Instant now = Instant.now();
        userTokenDao.insertSelectiveBatch(userTokenDoList);
        log.info("当前线程【{}】将segment数据对应的索引（{}条）插入到表中，耗时【{}】毫秒。", Thread.currentThread().getName(), userTokenDoList.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("将索引存储到数据库中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：flushToDB</B>
   * <B>概要说明：批量插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月19日 18:05:20
   * @Param []
   **/
  public void flushSegmentToDB(LinkedList<SegmentDo> segmentList) {
    if (0 < segmentList.size()) {
      try {
        Instant now = Instant.now();
        segmentDao.insertSelectiveBatch(segmentList);
        log.info("将【{}】条segment数据插入到表中，耗时【{}】毫秒。", segmentList.size(), DateTimeUtil.getTimeMillis(now));
        segmentList.clear();
      } catch (Exception e) {
        log.error("将segment数据批量插入到数据库中的时候，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSegmentDetailCountToRedis</B>
   * <B>概要说明：实时segmentDetail数据的统计数量保存到Redis的哈希表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月18日 16:07:28
   * @Param [count]
   **/
  public void flushSegmentDetailCountToRedis(LinkedList<MsSegmentDetailDo> list) {
    Instant now = Instant.now();
    if (null != list && 0 < list.size()) {
      Integer count = list.size();
      try {
        Map<String, Integer> map = new HashMap<>();
        for (MsSegmentDetailDo msSegmentDetailDo : list) {
          String userName = msSegmentDetailDo.getUserName();
          String startTime = msSegmentDetailDo.getStartTime();
          String tableName = msSegmentDetailDo.getMsTableName();
          String dbInstance = msSegmentDetailDo.getDbInstance();
          String peer = msSegmentDetailDo.getPeer();
          String serviceCode = msSegmentDetailDo.getServiceCode();
          if (StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(peer) && StringUtil.isNotBlank(dbInstance) && StringUtil.isNotBlank(tableName)) {
            // 信息概况 -> 用户访问行为
            flushUserAccessBehaviorToRedis(userName, startTime, peer, dbInstance, tableName, serviceCode);

            // 根据年月日，统计每天的访问次数；2022-07-20 14:11:55
            statisticVisitedCountByEveryday(msSegmentDetailDo, map);
          }
        }

        // 更新每天采集情况和总的采集情况到Redis；2022-07-20 14:17:03
        updateEverydayStatisticToRedis(map);

        log.info("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时统计【{}】条segmentDetail数据到Redis的哈希表中，耗时【{}】毫秒。", count, DateTimeUtil.getTimeMillis(now));
      } catch (Exception e) {
        log.error("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时segmentDetail数据的统计数量保存到Redis的哈希表中，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：updateEverydayStatisticToRedis</B>
   * <B>概要说明：更新每天采集情况和总的采集情况到Redis</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 14:07:45
   * @Param [map]
   **/
  private void updateEverydayStatisticToRedis(Map<String, Integer> map) {
    try {
      Iterator<String> iterator = map.keySet().iterator();
      while (iterator.hasNext()) {
        String key = iterator.next();
        Integer value = map.get(key);
        // 更新每天采集情况；
        redisPoolUtil.hsetIncrBy(Const.HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS, key, value.longValue());
        // 更新总的采集情况；
        redisPoolUtil.incr(Const.STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS, value.longValue());
      }
    } catch (Exception e) {
      log.error("# MingshiServerUtil.updateEverydayStatisticToRedis() # 更新每天采集情况和总的采集情况到Redis时，出现了异常。 ", e);
    }
  }

  /**
   * <B>方法名称：statisticVisitedCountByEveryday</B>
   * <B>概要说明：根据年月日，统计每天的访问次数；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 14:07:15
   * @Param [msSegmentDetailDo, map]
   **/
  private void statisticVisitedCountByEveryday(MsSegmentDetailDo msSegmentDetailDo, Map<String, Integer> map) {
    try {
      String startTimeOld = msSegmentDetailDo.getStartTime();
      Date date = DateTimeUtil.strToDate(startTimeOld);
      String startTimeNew = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);
      Integer value = map.get(startTimeNew);
      if (null == value) {
        map.put(startTimeNew, 1);
      } else {
        map.put(startTimeNew, value + 1);
      }
    } catch (Exception e) {
      log.error("# MingshiServerUtil.statisticVisitedCountByEveryday() #根据年月日，统计每天的访问次数时，出现了异常。 ", e);
    }
  }

  /**
   * <B>方法名称：flushUserNameToRedis</B>
   * <B>概要说明：将用户名发送到redis中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月19日 10:07:28
   * @Param [userHashSet]
   **/
  public void flushUserNameToRedis(HashSet<String> userHashSet) {
    if (null != userHashSet && 0 < userHashSet.size()) {
      Integer count = userHashSet.size();
      try {
        Instant now = Instant.now();
        for (String userName : userHashSet) {
          redisPoolUtil.sadd(Const.SET_DATA_STATISTICS_HOW_MANY_USERS, userName);
          // 将用户名放到本地内存中；2022-07-19 10:12:13
          InformationOverviewSingleton.put(userName);
        }
        log.info("# MingshiServerUtil.flushUserNameToRedis() # 实时统计将【{}】条用户名发送到redis中，耗时【{}】毫秒。", count, DateTimeUtil.getTimeMillis(now));
        userHashSet.clear();
      } catch (Exception e) {
        log.error("# MingshiServerUtil.flushUserNameToRedis() # 实时将用户名发送到redis中，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushUserAccessBehaviorToRedis</B>
   * <B>概要说明：实时将用户访问行为信息发送到redis中 </B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 14:07:54
   * @Param [userName, startTime, peer, dbInstance, tableName]
   **/
  private void flushUserAccessBehaviorToRedis(String userName, String startTime, String peer, String dbInstance, String tableName, String serviceCode) {
    try {
      // 用户访问次数 + 1；
      redisPoolUtil.incr(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + userName, 1);
      if (StringUtil.isNotBlank(startTime)) {
        // 更新用户对数据库最后的访问时间；
        redisPoolUtil.set(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + userName, startTime);
      }

      // 这里不再区分表名是否由多个组成，因为把表名存到MySQL数据库中就没有区分.这里强行区分的话，会导致Redis和MySQL中数据不一致。2022-07-21 08:49:12
      // 如果要换成ES查询，那可以将多个表名分开存储；2022-07-21 08:57:27
      doFlushUserAccessBehaviorToRedis(peer, dbInstance, tableName, userName, startTime, serviceCode);
      // if (tableName.contains(",")) {
      //   String[] split = tableName.split(",");
      //   for (String tableNameSplited : split) {
      //     doFlushUserAccessBehaviorToRedis(peer, dbInstance, tableNameSplited, userName, startTime);
      //   }
      // }else{
      //   doFlushUserAccessBehaviorToRedis(peer, dbInstance, tableName, userName, startTime);
      // }

    } catch (Exception e) {
      log.error("# MingshiServerUtil.doFlushUserAccessBehaviorToRedis() # 实时将用户访问行为信息发送到redis中，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：doFlushUserAccessBehaviorToRedis</B>
   * <B>概要说明：实时将用户访问行为信息发送到redis中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 17:07:45
   * @Param [peer, dbInstance, tableName, userName, startTime]
   **/
  private void doFlushUserAccessBehaviorToRedis(String peer, String dbInstance, String tableName, String userName, String startTime, String serviceCode) {
    // 累加用户对数据库表资源的访问次数；
    String zsetVlue = doGetTableName(peer, dbInstance, tableName);
    String serviceCodeName = AgentInformationSingleton.get(serviceCode);
    serviceCode = serviceCodeName == Const.DOLLAR ? serviceCode : serviceCodeName;

    if (tableName.contains(",")) {
      String[] split = tableName.split(",");
      for (String tn : split) {
        // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
        zsetVlue = doGetTableName(peer, dbInstance, tn);
        // 将用户访问过的表放到这个用户对应的有序集合zset中；2022-07-20 14:30:07
        redisPoolUtil.incrementScore(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName, zsetVlue, 1);
        // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
        redisPoolUtil.incrementScore(Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue, serviceCode + Const.DOLLAR + userName, 1);
        // 记录每一个数据库表最后的被访问的时间；
        redisPoolUtil.set(Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue, startTime);
        // redisPoolUtil.incrementScore(Const.ZSET_HOW_MANY_TABLES, zsetVlue, 0);
        LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(zsetVlue, true);
      }
    } else {
      // 将用户访问过的表放到这个用户对应的有序集合zset中；2022-07-20 14:30:07
      redisPoolUtil.incrementScore(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName, zsetVlue, 1);
      // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
      redisPoolUtil.incrementScore(Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue, serviceCode + Const.DOLLAR + userName, 1);
      // 记录每一个数据库表最后的被访问的时间；
      redisPoolUtil.set(Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue, startTime);
      // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
      // redisPoolUtil.incrementScore(Const.ZSET_HOW_MANY_TABLES, zsetVlue, 0);
      LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(zsetVlue, true);
    }

  }

  /**
   * <B>方法名称：flushAuditLogToDB</B>
   * <B>概要说明：将来自探针的SQL语句插入到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月30日 18:05:15
   * @Param [now]
   **/
  public void flushAuditLogToDB(LinkedList<MsAuditLogDo> auditLogList) {
    if (0 < auditLogList.size()) {
      try {
        Instant now = Instant.now();
        msAuditLogDao.insertSelectiveBatch(auditLogList);
        log.info("#SegmentConsumeServiceImpl.reorganizingSpans()# 将来自探针的【{}】条SQL语句插入到表中耗时【{}】毫秒。", auditLogList.size(), DateTimeUtil.getTimeMillis(now));
        auditLogList.clear();
      } catch (Exception e) {
        log.error("#SegmentConsumeServiceImpl.reorganizingSpans()# 将来自探针的SQL语句插入到表中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushAbnormalToDB</B>
   * <B>概要说明：将异常信息批量插入到MySQL中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月07日 18:06:24
   * @Param [segmentDetaiDolList]
   **/
  public void flushAbnormalToDB(List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist) {
    if (0 < msAlarmInformationDoLinkedListist.size()) {
      try {
        Instant now = Instant.now();
        msAlarmInformationMapper.insertSelectiveBatch(msAlarmInformationDoLinkedListist);
        log.info("#SegmentConsumeServiceImpl.flushAbnormalToDB()# 将异常信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", msAlarmInformationDoLinkedListist.size(), DateTimeUtil.getTimeMillis(now));
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushAbnormalToDB() # 将异常信息批量插入到MySQL中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSkywalkingAgentNameToRedis</B>
   * <B>概要说明：将探针信息发送到MySQL中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月27日 13:06:22
   * @Param [segmentDetailDoList]
   **/
  public void flushSkywalkingAgentInformationToDb() {
    try {
      AtomicBoolean atomicBoolean = AgentInformationSingleton.getAtomicBoolean();
      if (atomicBoolean.get() == false) {
        // 只有当数据有变动时，才将其刷入到数据库中；2022-06-28 17:35:54
        return;
      }
      CopyOnWriteMap<String, String> instance = AgentInformationSingleton.getInstance();
      if (null != instance && 0 < instance.size()) {
        Instant now = Instant.now();
        LinkedList<MsAgentInformationDo> list = new LinkedList<>();
        Iterator<String> iterator = instance.keySet().iterator();
        while (iterator.hasNext()) {
          String key = iterator.next();
          if(StringUtil.isBlank(key)){
            continue;
          }
          MsAgentInformationDo msAgentInformationDo = new MsAgentInformationDo();
          msAgentInformationDo.setAgentCode(key);
          list.add(msAgentInformationDo);
        }
        msAgentInformationMapper.insertBatch(list);
        // 本次刷新过后，只有当真的有数据变更后，下次才将其刷入到MySQL中；2022-06-28 17:51:11
        AgentInformationSingleton.setAtomicBooleanToFalse();
        log.info("#SegmentConsumeServiceImpl.flushSkywalkingAgentInformationToDb()# 将探针名称信息【{}条】批量插入到MySQL数据库中耗时【{}】毫秒。", instance.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.flushSkywalkingAgentInformationToDb() # 将探针名称信息批量插入到MySQL数据库中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：flushQpsToRedis</B>
   * <B>概要说明：将QPS信息发送到Redis中</B>
   * @Author zm
   * @Date 2022年06月28日 18:06:41
   * @Param []
   * @return void
   **/
  // public void flushQpsToRedis() {
  //   Map<String, AtomicInteger> timeCountMap = StatisticsConsumeProcessorThreadQPS.getTimeCountMap();
  //   if (null != timeCountMap && 0 < timeCountMap.size()) {
  //     try {
  //       Map<String, Integer> countMap = new HashMap<>();
  //       // Map<String, String> countMap = new HashMap<>();
  //       Iterator<String> iterator = timeCountMap.keySet().iterator();
  //       while(iterator.hasNext()){
  //         String key = iterator.next();
  //         AtomicInteger value = timeCountMap.get(key);
  //         countMap.put(key,value.intValue());
  //       }
  //       Instant now = Instant.now();

  //       redisPoolUtil.hsetBatch2(Const.SKYWALKING_CONSUME_QPS, countMap);
  //       log.info("#SegmentConsumeServiceImpl.flushQpsToRedis()# 将QPS信息【{}条】发送到Redis中耗时【{}】毫秒。", timeCountMap.size(), DateTimeUtil.getTimeMillis(now));
  //     } catch (Exception e) {
  //       log.error("# SegmentConsumeServiceImpl.flushQpsToRedis() # 将QPS批量发送到Redis中出现了异常。", e);
  //     }
  //   }
  // }

  /**
   * <B>方法名称：flushSkywalkingAgentNameToRedis</B>
   * <B>概要说明：将探针信息发送到Redis中，用于计算探针心跳</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月27日 13:06:22
   * @Param [segmentDetailDoList]
   **/
  public void flushSkywalkingAgentNameToRedis(Map<String, String> map) {
    if (null != map && 0 < map.size()) {
      try {
        Instant now = Instant.now();
        redisPoolUtil.hsetBatch(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, map);
        log.info("#SegmentConsumeServiceImpl.flushSkywalkingAgentNameToRedis()# 将探针名称信息【{}】【{}条】批量插入到Redis中耗时【{}】毫秒。", JsonUtil.obj2String(map), map.size(), DateTimeUtil.getTimeMillis(now));
        map.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSkywalkingAgentNameToRedis() # 将探针名称信息批量插入到Redis中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSegmentDetailToDB</B>
   * <B>概要说明：将segmentDetail实例信息批量插入到MySQL中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月02日 11:06:24
   * @Param [segmentDetaiDolList]
   **/
  public void flushSegmentDetailToEs(LinkedList<EsMsSegmentDetailDo> segmentDetailDoList) {
    if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {
      try {
        Instant now = Instant.now();
        mingshiElasticSearchUtil.saveAll(segmentDetailDoList);
        log.info("#SegmentConsumeServiceImpl.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到ES中耗时【{}】毫秒。", segmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
        segmentDetailDoList.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSegmentDetailToDB() # 将segmentDetail实例信息批量插入到ES中出现了异常。", e);
      }
    }
  }

  public void flushSegmentDetailCountToEs(LinkedList<EsMsSegmentDetailDo> segmentDetailDoList) {
    if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {
      try {
        Instant now = Instant.now();
        mingshiElasticSearchUtil.saveAll(segmentDetailDoList);
        log.info("#SegmentConsumeServiceImpl.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到ES中耗时【{}】毫秒。", segmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
        segmentDetailDoList.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSegmentDetailToDB() # 将segmentDetail实例信息批量插入到ES中出现了异常。", e);
      }
    }
  }

  @Transactional
  public void flushSegmentDetailToDB(LinkedList<MsSegmentDetailDo> segmentDetailDoList) {
    if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {
      try {
        Instant now = Instant.now();
        msSegmentDetailDao.insertSelectiveBatch(segmentDetailDoList);

        // 实时segmentDetail数据的统计数量保存到Redis的哈希表中
        flushSegmentDetailCountToRedis(segmentDetailDoList);

        log.info("#SegmentConsumeServiceImpl.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", segmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
        segmentDetailDoList.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSegmentDetailToDB() # 将segmentDetail实例信息批量插入到MySQL中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSpansToDB</B>
   * <B>概要说明：将Spans实例信息插入到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月18日 09:07:06
   * @Param [spansList]
   **/
  public void flushSpansToDB(List<Span> spansList) {
    if (null != spansList && 0 < spansList.size()) {
      try {
        Instant now = Instant.now();
        spanMapper.insertSelectiveBatch(spansList);
        log.info("#SegmentConsumeServiceImpl.flushSpansToDB()# 将Spans实例信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", spansList.size(), DateTimeUtil.getTimeMillis(now));
        spansList.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSpansToDB() # 将Spans实例信息批量插入到MySQL中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：getTableName</B>
   * <B>概要说明：获取表名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年07月15日 11:07:56
   * @Param [msMonitorBusinessSystemTablesDo]
   **/
  public String getTableName(MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo) {
    String key = "";
    if (null != msMonitorBusinessSystemTablesDo) {
      String dbAddress = msMonitorBusinessSystemTablesDo.getDbAddress();
      String dbName = msMonitorBusinessSystemTablesDo.getDbName();
      String tableName = msMonitorBusinessSystemTablesDo.getTableName();
      return doGetTableName(dbAddress, dbName, tableName);
    }
    return key;
  }

  public String doGetTableName(String dbAddress, String dbName, String tableName) {
    String key = "";
    if (StringUtil.isNotBlank(dbAddress)) {
      key = dbAddress + "#";
    }
    if (StringUtil.isNotBlank(dbName)) {
      key += dbName + "#";
    }
    if (StringUtil.isNotBlank(tableName)) {
      key += tableName;
    }
    return key;
  }

  /**
   * <B>方法名称：insertMonitorTables</B>
   * <B>概要说明：将业务系统中不存在的表批量插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月13日 14:07:03
   * @Param []
   **/
  public void insertMonitorTables() {
    try {
      Map<String, Integer> isChangedMap = LoadAllEnableMonitorTablesFromDb.getIsChangedMap();
      if (null != isChangedMap && 0 < isChangedMap.size()) {
        Set<String> keySet = isChangedMap.keySet();
        LinkedList<MsMonitorBusinessSystemTablesDo> list = new LinkedList<>();
        for (String tables : keySet) {
          String[] splits = tables.split("#");
          if (3 == splits.length) {
            MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo = new MsMonitorBusinessSystemTablesDo();
            msMonitorBusinessSystemTablesDo.setDbAddress(splits[0]);
            msMonitorBusinessSystemTablesDo.setDbName(splits[1]);
            msMonitorBusinessSystemTablesDo.setTableName(splits[2]);
            list.add(msMonitorBusinessSystemTablesDo);
          }
        }
        if (0 < list.size()) {
          msMonitorBusinessSystemTablesMapper.insertSelectiveBatch(list);
        }
        isChangedMap.clear();
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.insertMonitorTables() # 将监管表中不存在的表插入到监管表中，出现了异常。", e);
    }
  }
}
