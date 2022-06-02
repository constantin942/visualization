package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.config.SingletonLocalStatisticsMap;
import com.mingshi.skyflying.dao.MsAuditLogDao;
import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.UserTokenDao;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.domain.UserTokenDo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private MsAuditLogDao msAuditLogDao;
  @Resource
  private SegmentDao segmentDao;
  @Resource
  private UserTokenDao userTokenDao;


  /**
   * <B>方法名称：flushSegmentIndexToDB</B>
   * <B>概要说明：将索引保存到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月24日 11:05:07
   * @Param [now]
   **/
  public void flushSegmentIndexToDB() {
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
    List<UserTokenDo> userTokenDoList = new LinkedList<>();
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
        log.error("# IoThread.flushSegmentIndexToDB # 将索引插入到数据库中的时候，出现了异常。userName = null，globalTraceId = 【{}】，token = 【{}】。", globalTraceId, token);
        continue;
      }

      UserTokenDo userTokenDo = new UserTokenDo();
      userTokenDo.setUserName(userName);
      userTokenDo.setGlobalTraceId(globalTraceId);
      userTokenDo.setToken(token);
      userTokenDoList.add(userTokenDo);

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
    batchInsertUserToken(userTokenDoList);

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
   * <B>方法名称：flushSegmentDetailToDB</B>
   * <B>概要说明：将segmentDetail实例信息批量插入到MySQL中</B>
   * @Author zm
   * @Date 2022年06月02日 11:06:24
   * @Param [segmentDetaiDolList]
   * @return void
   **/
  public void flushSegmentDetailToDB(LinkedList<MsSegmentDetailDo> segmentDetailDoList) {
    if (0 < segmentDetailDoList.size()) {
      try {
        Instant now = Instant.now();
        msSegmentDetailDao.insertSelectiveBatch(segmentDetailDoList);
        log.info("#SegmentConsumeServiceImpl.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", segmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
        segmentDetailDoList.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSegmentDetailToDB() # 将segmentDetail实例信息批量插入到MySQL中出现了异常。", e);
      }
    }
  }
}
