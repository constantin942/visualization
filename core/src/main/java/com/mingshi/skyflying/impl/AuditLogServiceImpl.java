package com.mingshi.skyflying.impl;


import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.metadata.CellExtra;
import com.alibaba.excel.read.listener.ReadListener;
import com.alibaba.fastjson.JSONObject;
import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogRequest;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogResponse;
import com.aliyuncs.profile.DefaultProfile;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.AllAuditLogDao;
import com.mingshi.skyflying.dao.MsAuditLogDao;
import com.mingshi.skyflying.dao.MsConfigDao;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.domain.AllAuditLogDo;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.domain.MsConfigDo;
import com.mingshi.skyflying.domain.MsScheduledTaskDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.ListUtils;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.LinkedList;
import java.util.List;

/**
 * <B>方法名称：AuditLogServiceImpl</B>
 * <B>概要说明：将审计日志插入到数据库中</B>
 *
 * @Author zm
 * @Date 2022年05月25日 14:05:13
 * @Param
 * @return
 **/
@Slf4j
@Service("auditLogService")
public class AuditLogServiceImpl implements AuditLogService {

  @Resource
  private MsAuditLogDao msAuditLogDao;
  @Resource
  private MsScheduledTaskDao msScheduledTaskDao;
  @Resource
  private MsConfigDao msConfigDao;
  @Resource
  private AllAuditLogDao allAuditLogDao;

  @Override
  public ServerResponse<String> process(List<ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog> listSQLExecAuditLogList) {
    int size = 0;
    Instant now = Instant.now();
    log.info("开始执行# AuditLogServiceImpl.process() # 将阿里云提供的数据库审计日志插入到数据库中。");
    try {
      List<MsAuditLogDo> list = new LinkedList<>();
      for (ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog dmsAuditLogDo : listSQLExecAuditLogList) {
        String schemaName = dmsAuditLogDo.getSchemaName();
        if (schemaName.equals("mysql")) {
          continue;
        }
        String sql = dmsAuditLogDo.getSQL();
        if (sql.startsWith("show variables") || sql.startsWith("set global") || sql.startsWith("select @@version")) {
          continue;
        }
        MsAuditLogDo msAuditLogDo = new MsAuditLogDo();
        msAuditLogDo.setMsSql(sql);
        msAuditLogDo.setMsSchemaName(schemaName);
        msAuditLogDo.setAffectRows(dmsAuditLogDo.getAffectRows());
        msAuditLogDo.setOpTime(dmsAuditLogDo.getOpTime());
        msAuditLogDo.setUserName(dmsAuditLogDo.getUserName());
        msAuditLogDo.setUserId(dmsAuditLogDo.getUserId());
        msAuditLogDo.setInstanceName(dmsAuditLogDo.getInstanceName());
        msAuditLogDo.setInstanceId(dmsAuditLogDo.getInstanceId());
        msAuditLogDo.setDbId(dmsAuditLogDo.getDbId());
        msAuditLogDo.setLogic(dmsAuditLogDo.getLogic());
        msAuditLogDo.setSqlType(dmsAuditLogDo.getSQLType());
        msAuditLogDo.setExecState(dmsAuditLogDo.getExecState());
        msAuditLogDo.setAffectRows(dmsAuditLogDo.getAffectRows());
        msAuditLogDo.setElapsedTime(dmsAuditLogDo.getElapsedTime());
        msAuditLogDo.setRemark(dmsAuditLogDo.getRemark());

        list.add(msAuditLogDo);
      }
      size = list.size();
      if (0 < list.size()) {
        msAuditLogDao.insertSelectiveBatch(list);
      }
    } catch (Exception e) {
      log.error("将阿里云提供的数据库审计日志插入到表中出现了异常。", e);
    }
    log.info("执行完毕# AuditLogServiceImpl.process() # 将阿里云提供的数据库审计日志【{}条】插入到数据库中。耗时【{}】毫秒。", size, DateTimeUtil.getTimeMillis(now));
    return null;
  }

  /**
   * <B>方法名称：autoFetchAuditlogByDMS</B>
   * <B>概要说明：通过定时任务，自动拉取MDS中的数据库审计日志</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 15:05:40
   * @Param []
   **/
  @Override
  public ServerResponse<String> autoFetchAuditlogByDMS(String startTime, String endTime) {
    Instant now = Instant.now();
    if (StringUtil.isBlank(startTime) || StringUtil.isBlank(endTime)) {
      log.error("#AuditLogServiceImpl.manualFetchAuditlog()# 通过定时任务自动拉取DMS审计日志，收到的开始间或者结束时间为空。");
      return ServerResponse.createByErrorMessage("开始时间或者结束时间不能为空", "");
    }
    log.info("开始执行 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS审计日志。参数 startTime = 【{}】，endTime = 【{}】。", startTime, endTime);
    MsConfigDo msConfigDo = msConfigDao.selectByConfigType(Const.AK_SK);
    if (null == msConfigDo || StringUtil.isBlank(msConfigDo.getConfig())) {
      log.error("#AuditLogServiceImpl.manualFetchAuditlog()# 通过定时任务自动拉取DMS审计日志时，在数据库中没有获取到aksk配置。");
      return ServerResponse.createByErrorMessage("没有配置aksk信息", "");
    }
    String config = null;
    String ak = null;
    String sk = null;
    try {
      config = msConfigDo.getConfig();
      JSONObject jsonObject = JSONObject.parseObject(config);
      ak = jsonObject.getString(Const.AK);
      sk = jsonObject.getString(Const.SK);
      if (StringUtil.isBlank(ak) || StringUtil.isBlank(sk)) {
        log.error("#AuditLogServiceImpl.manualFetchAuditlog()# 通过定时任务自动拉取DMS审计日志时，在数据库中没有获取到ak或者sk配置。");
        return ServerResponse.createByErrorMessage("没有配置ak或者sk信息", "");
      }
    } catch (Exception e) {
      log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志，解析在数据库中获取到的aksk配置 = 【{}】时，出现了异常。", config, e);
      return ServerResponse.createByErrorMessage("解析aksk配置信息时，出现了异常。" + e.getMessage(), "");
    }

    DefaultProfile profile = DefaultProfile.getProfile("cn-beijing", ak, sk);
    IAcsClient client = new DefaultAcsClient(profile);

    ServerResponse<String> dmsAuditLog = getDMSAuditLog(client, startTime, endTime);
    log.info("执行结束 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志。耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
    return dmsAuditLog;

  }

  /**
   * <B>方法名称：getAuditlogByExcel</B>
   * <B>概要说明：从excel表中读取审计日志</B>
   *
   * @param path
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 19:05:57
   * @Param []
   */
  @Override
  public ServerResponse<String> getAuditlogByExcel(String path) {
    EasyExcel.read(path, AllAuditLogDo.class, new ReadListener<AllAuditLogDo>() {
      /**
       * 单次缓存的数据量
       */
      public static final int BATCH_COUNT = 1000;
      /**
       *临时存储
       */
      private List<MsAuditLogDo> cachedDataList = ListUtils.newArrayListWithExpectedSize(BATCH_COUNT);

      @Override
      public void onException(Exception e, AnalysisContext analysisContext) {
      }

      @Override
      public void invoke(AllAuditLogDo data, AnalysisContext context) {
        if (data.getLOG().contains("select 1")) {
          // System.out.println("出现 select 1 的SQL语句了。");
        } else if (data.getLOG().startsWith("SHOW")) {
          // System.out.println("出现 SHOW 的SQL语句了。");
        } else if (data.getLOG().startsWith("SET PROFILING = 1")) {
          // System.out.println("出现 SET PROFILING = 1 的SQL语句了。");
        } else if (data.getLOG().startsWith("SELECT QUERY_ID, SUM(DURATION)")) {
          // System.out.println("出现 SELECT QUERY_ID, SUM(DURATION) 的SQL语句了。");
        } else if (data.getLOG().startsWith("SELECT STATE AS ")) {
          // System.out.println("出现 SELECT STATE AS  的SQL语句了。");
        } else if (data.getLOG().startsWith("select table_name")) {
          // System.out.println("出现 select table_name 的SQL语句了。");
        } else if (data.getLOG().startsWith("SELECT DISTINCT table_name, ")) {
          // System.out.println("出现 SELECT DISTINCT table_name, 的SQL语句了。");
        } else if (data.getLOG().startsWith("SET") || data.getLOG().startsWith("set")) {
          // System.out.println("出现 SET或者set 的SQL语句了。");
        } else if (data.getLOG().startsWith("COMMIT") || data.getLOG().startsWith("commit")) {
          // System.out.println("出现 COMMIT或者commit 的SQL语句了。");
        } else if (data.getLOG().startsWith("SELECT @@sessi")) {
          // System.out.println("出现 SELECT @@session.tx_read_only 的SQL语句了。");
        } else if (data.getLOG().startsWith("logout")) {
          // System.out.println("出现 logout 的SQL语句了。");
        } else if (data.getLOG().startsWith("update hy_company")) {
          // System.out.println("出现 update hy_company 的SQL语句了。");
        } else if (data.getLOG().startsWith("login")) {
          // System.out.println("出现 login 的SQL语句了。");
        }
        else {
          Long origin_time = Long.valueOf(data.getORIGIN_TIME()) / 1000;
          MsAuditLogDo msAuditLogDo = new MsAuditLogDo();
          String msSql = data.getLOG();
          if (msSql.contains("DMS")) {
            msSql = data.getLOG().split("/*/")[2];
            msAuditLogDo.setSqlSource(Const.SQL_SOURCE_DMS);
          } else {
            msAuditLogDo.setSqlSource(Const.SQL_SOURCE_INSIGHT);
          }
          msAuditLogDo.setMsSql(msSql);
          String opTime = DateTimeUtil.longToDate(origin_time);
          msAuditLogDo.setOpTime(opTime);
          String msSchemaName = data.getDB();
          msAuditLogDo.setMsSchemaName(msSchemaName);
          msAuditLogDo.setSqlInsightDbUserName(data.getUSER());
          msAuditLogDo.setSqlInsightUserIp(data.getUSER_IP());
          String sqlType = data.getSQL_TYPE();
          msAuditLogDo.setSqlType(sqlType);
          String strData = StringUtil.recombination(msSql, opTime, msSchemaName, sqlType);
          String hash = StringUtil.MD5(strData);
          msAuditLogDo.setHash(hash);

          cachedDataList.add(msAuditLogDo);
          if (cachedDataList.size() >= BATCH_COUNT) {
            saveData();
            // 存储完成清理 list
            cachedDataList = ListUtils.newArrayListWithExpectedSize(BATCH_COUNT);
          }
        }
      }

      @Override
      public void extra(CellExtra cellExtra, AnalysisContext analysisContext) {
      }

      @Override
      public void doAfterAllAnalysed(AnalysisContext context) {
        saveData();
      }

      @Override
      public boolean hasNext(AnalysisContext analysisContext) {
        return true;
      }

      /**
       * 加上存储数据库
       */
      private void saveData() {
        Instant now = Instant.now();
        // log.info("{}条数据，开始存储数据库！", cachedDataList.size());
        try {
          msAuditLogDao.insertSelectiveBatch(cachedDataList);
        } catch (Exception e) {
          log.error("# # 将SQL洞察中的审计日志批量插入到数据库中，出现了异常。", e);
        }
        log.info("存储【{}】条数据成功！耗时【{}】毫秒。",cachedDataList.size(),DateTimeUtil.getTimeMillis(now));
      }
    }).sheet().doRead();
    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：getDMSAuditLog</B>
   * <B>概要说明：循环拉取DMS中的审计日志</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 16:05:31
   * @Param [client, startTime, endTime]
   **/
  private ServerResponse<String> getDMSAuditLog(IAcsClient client, String startTime, String endTime) {
    ServerResponse<String> byErrorMessage = ServerResponse.createBySuccess();
    int pageSize = 1;
    int pageNumber = 0;

    int increment = 0;
    Boolean flag = true;
    while (true == flag) {
      ListSQLExecAuditLogRequest request = new ListSQLExecAuditLogRequest();
      request.setStartTime(startTime);
      request.setEndTime(endTime);
      request.setPageSize(pageSize);
      request.setPageNumber(++pageNumber);
      MsScheduledTaskDo msScheduledTaskDo = new MsScheduledTaskDo();

      try {
        ListSQLExecAuditLogResponse response = client.getAcsResponse(request);
        Long totalCount = response.getTotalCount();
        List<ListSQLExecAuditLogResponse.SQLExecAuditLog> sqlExecAuditLogList = response.getSQLExecAuditLogList();
        increment += sqlExecAuditLogList.size();

        if (totalCount.intValue() == increment) {
          // 当本次拉取的审计信息小于每页设置的大小时，则说明这个时间段内的数据已经被拉取完毕了，那么就退出循环。2022-05-26 16:29:47
          flag = false;
        }
        batchProcessDMSAuditLog(sqlExecAuditLogList);

        msScheduledTaskDo.setStartTime(startTime);
        msScheduledTaskDo.setEndTime(endTime);
        msScheduledTaskDo.setPageSize(pageSize);
        msScheduledTaskDo.setPageNumber(pageNumber);
        msScheduledTaskDo.setStatus(Const.RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT);
        msScheduledTaskDo.setRecordCount(sqlExecAuditLogList.size());
      } catch (Exception e) {
        msScheduledTaskDo.setStatus(Const.RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT);
        log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志时，出现了异常。", e);
        byErrorMessage = ServerResponse.createByErrorMessage("拉取DMS审计日志时，出现了异常。" + e.getMessage(), "");
      }
      int insertResult = msScheduledTaskDao.insertSelective(msScheduledTaskDo);
      if (1 != insertResult) {
        log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志，把拉取到的数据 = 【{}】存入到数据库失败。", JsonUtil.obj2String(msScheduledTaskDo));
      }
    }

    return byErrorMessage;
  }

  public ServerResponse<String> batchProcessDMSAuditLog(List<ListSQLExecAuditLogResponse.SQLExecAuditLog> listSQLExecAuditLogList) {
    int size = 0;
    Instant now = Instant.now();
    log.info("开始执行# AuditLogServiceImpl.batchProcessDMSAuditLog() # 将阿里云提供的DMS数据库审计日志插入到数据库中。");
    try {
      List<MsAuditLogDo> list = new LinkedList<>();
      for (ListSQLExecAuditLogResponse.SQLExecAuditLog sqlExecAuditLog : listSQLExecAuditLogList) {
        String msSchemaName = sqlExecAuditLog.getSchemaName();
        if (msSchemaName.equals("mysql")) {
          continue;
        }
        String msSql = sqlExecAuditLog.getSQL();
        if (msSql.startsWith("show variables") || msSql.startsWith("set global") || msSql.startsWith("select @@version")) {
          continue;
        }

        String opTime = sqlExecAuditLog.getOpTime();
        String userName = sqlExecAuditLog.getUserName();
        Long userId = sqlExecAuditLog.getUserId();
        String instanceName = sqlExecAuditLog.getInstanceName();
        Long instanceId = sqlExecAuditLog.getInstanceId();
        Long dbId = sqlExecAuditLog.getDbId();
        Boolean logic = sqlExecAuditLog.getLogic();
        String sqlType = sqlExecAuditLog.getSQLType();
        String execState = sqlExecAuditLog.getExecState();
        Long affectRows = sqlExecAuditLog.getAffectRows();
        Long elapsedTime = sqlExecAuditLog.getElapsedTime();
        String remark = sqlExecAuditLog.getRemark();

        // 大写统一转换成消息、去掉多余的空格
        String strData = StringUtil.recombination(msSql , opTime, msSchemaName , sqlType);
        String hash = StringUtil.MD5(strData);
        // String hash = StringUtil.MD5(opTime + userName + userId + instanceName + instanceId + dbId + logic + sqlType + execState + affectRows + elapsedTime + remark);

        MsAuditLogDo msAuditLogDo = new MsAuditLogDo();
        msAuditLogDo.setMsSql(msSql);
        msAuditLogDo.setSqlSource(Const.SQL_SOURCE_DMS);
        msAuditLogDo.setMsSchemaName(msSchemaName);
        msAuditLogDo.setOpTime(opTime);
        msAuditLogDo.setUserName(userName);
        msAuditLogDo.setUserId(userId);
        msAuditLogDo.setInstanceName(instanceName);
        msAuditLogDo.setInstanceId(instanceId);
        msAuditLogDo.setDbId(dbId);
        msAuditLogDo.setLogic(logic);
        msAuditLogDo.setSqlType(sqlType);
        msAuditLogDo.setExecState(execState);
        msAuditLogDo.setAffectRows(affectRows);
        msAuditLogDo.setElapsedTime(elapsedTime);
        msAuditLogDo.setRemark(remark);
        msAuditLogDo.setHash(hash);

        list.add(msAuditLogDo);
      }
      size = list.size();
      if (0 < list.size()) {
        msAuditLogDao.insertSelectiveBatch(list);
      }
    } catch (Exception e) {
      log.error("将阿里云提供的DMS数据库审计日志插入到表中出现了异常。", e);
    }
    log.info("执行完毕# AuditLogServiceImpl.batchProcessDMSAuditLog() # 将阿里云提供的DMS数据库审计日志【{}条】插入到数据库中。耗时【{}】毫秒。", size, DateTimeUtil.getTimeMillis(now));
    return null;
  }
}
