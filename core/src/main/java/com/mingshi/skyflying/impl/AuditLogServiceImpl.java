package com.mingshi.skyflying.impl;


import com.alibaba.fastjson.JSONObject;
import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogRequest;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogResponse;
import com.aliyuncs.profile.DefaultProfile;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.MsAuditLogDao;
import com.mingshi.skyflying.dao.MsConfigDao;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.domain.MsConfigDo;
import com.mingshi.skyflying.domain.MsScheduledTaskDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
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
   * <B>方法名称：manualFetchAuditlog</B>
   * <B>概要说明：手动全量拉取宝拉（polar）数据库的审计日志</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 15:05:40
   * @Param []
   **/
  @Override
  public ServerResponse<String> manualFetchAuditlog() {
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
    if(StringUtil.isBlank(startTime) || StringUtil.isBlank(endTime)){
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
   * <B>方法名称：getDMSAuditLog</B>
   * <B>概要说明：循环拉取DMS中的审计日志</B>
   * @Author zm
   * @Date 2022年05月26日 16:05:31
   * @Param [client, startTime, endTime]
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
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
      for (ListSQLExecAuditLogResponse.SQLExecAuditLog dmsAuditLogDo : listSQLExecAuditLogList) {
        String msSchemaName = dmsAuditLogDo.getSchemaName();
        if (msSchemaName.equals("mysql")) {
          continue;
        }
        String msSql = dmsAuditLogDo.getSQL();
        if (msSql.startsWith("show variables") || msSql.startsWith("set global") || msSql.startsWith("select @@version")) {
          continue;
        }

        String opTime = dmsAuditLogDo.getOpTime();
        String userName = dmsAuditLogDo.getUserName();
        Long userId = dmsAuditLogDo.getUserId();
        String instanceName = dmsAuditLogDo.getInstanceName();
        Long instanceId = dmsAuditLogDo.getInstanceId();
        Long dbId = dmsAuditLogDo.getDbId();
        Boolean logic = dmsAuditLogDo.getLogic();
        String sqlType = dmsAuditLogDo.getSQLType();
        String execState = dmsAuditLogDo.getExecState();
        Long affectRows = dmsAuditLogDo.getAffectRows();
        Long elapsedTime = dmsAuditLogDo.getElapsedTime();
        String remark = dmsAuditLogDo.getRemark();

        String hash = StringUtil.MD5(opTime + userName + userId + instanceName + instanceId + dbId + logic + sqlType + execState + affectRows + elapsedTime + remark);

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
