package com.mingshi.skyflying.impl;

import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogRequest;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogResponse;
import com.aliyuncs.profile.DefaultProfile;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsConfigDo;
import com.mingshi.skyflying.common.domain.MsDmsAuditLogDo;
import com.mingshi.skyflying.common.domain.MsScheduledTaskDo;
import com.mingshi.skyflying.common.enums.SqlType;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsConfigDao;
import com.mingshi.skyflying.dao.MsDmsAuditLogDao;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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
  private MingshiServerUtil mingshiServerUtil;
  @Resource
  private MsDmsAuditLogDao msDmsAuditLogDao;
  @Resource
  private MsScheduledTaskDao msScheduledTaskDao;
  @Resource
  private MsConfigDao msConfigDao;

  /**
   * <B>方法名称：autoFetchAuditlogByDMS</B>
   * <B>概要说明：通过定时任务，自动拉取MDS中的数据库审计日志</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 15:05:40
   * @Param []
   **/
  @Override
  public ServerResponse<String> autoFetchAuditlogByDms(String startTime, String endTime) {
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

      ObjectNode jsonObject = JsonUtil.string2Object(config, ObjectNode.class);
      ak = jsonObject.get(Const.AK).asText();
      sk = jsonObject.get(Const.SK).asText();
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

    ServerResponse<String> dmsAuditLog = getDmsAuditLog(client, startTime, endTime);
    log.info("执行结束 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志。耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
    return dmsAuditLog;

  }

  /**
   * <B>方法名称：getDMSAuditLog</B>
   * <B>概要说明：循环拉取DMS中的审计日志</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 16:05:31
   * @Param [client, startTime, endTime]
   **/
  private ServerResponse<String> getDmsAuditLog(IAcsClient client, String startTime, String endTime) {
    ServerResponse<String> byErrorMessage = ServerResponse.createBySuccess();
    int pageSize = 100;
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
        log.info("# AuditLogServiceImpl.autoFetchAuditlogByDMS() # 通过定时任务自动拉取DMS的审计日志，终止死循环条件是totalCount.intValue() == increment，" +
          "当前totalCount.intValue() =【{}】，increment = 【{}】。", totalCount.intValue(), increment);

        if (0 < sqlExecAuditLogList.size()) {
          batchProcessDmsAuditLog(sqlExecAuditLogList);
        }

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
      if (StringUtil.isNotBlank(msScheduledTaskDo.getStartTime()) &&
        StringUtil.isNotBlank(msScheduledTaskDo.getEndTime()) &&
        null != msScheduledTaskDo.getPageNumber() &&
        null != msScheduledTaskDo.getPageSize() &&
        null != msScheduledTaskDo.getRecordCount() && 0 < msScheduledTaskDo.getRecordCount()) {
        int insertResult = msScheduledTaskDao.insertSelective(msScheduledTaskDo);
        if (1 != insertResult) {
          log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志，把拉取到的数据 = 【{}】存入到数据库失败。", JsonUtil.obj2String(msScheduledTaskDo));
        }
      }
    }

    return byErrorMessage;
  }

  public ServerResponse<String> batchProcessDmsAuditLog(List<ListSQLExecAuditLogResponse.SQLExecAuditLog> listSqlExecAuditLogList) {
    int size = 0;
    Instant now = Instant.now();
    log.info("开始执行# AuditLogServiceImpl.batchProcessDMSAuditLog() # 将阿里云提供的DMS数据库审计日志插入到数据库中。");
    try {
      List<MsDmsAuditLogDo> list = new LinkedList<>();
      for (ListSQLExecAuditLogResponse.SQLExecAuditLog sqlExecAuditLog : listSqlExecAuditLogList) {
        String msSchemaName = sqlExecAuditLog.getSchemaName();
        if (Const.MYSQL.equals(msSchemaName)) {
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
        String strData = StringUtil.recombination(msSql, opTime, msSchemaName, sqlType);
        String hash = StringUtil.mD5(strData);
        MsDmsAuditLogDo msDmsAuditLogDo = new MsDmsAuditLogDo();

        String sqlType1 = mingshiServerUtil.getSqlType(msSql);
        // 获取表名；2022-06-06 14:11:21
        if (!sqlType1.equals(SqlType.NONE.toString().trim()) && StringUtil.isNotBlank(sqlType1) && !Const.FAIL.equals(execState)) {
          String tableName = mingshiServerUtil.getTableName(sqlType1, msSql);
          msDmsAuditLogDo.setMsTableName(tableName);
        }


        msDmsAuditLogDo.setMsSql(msSql);
        msDmsAuditLogDo.setSqlSource(Const.SQL_SOURCE_DMS);
        msDmsAuditLogDo.setMsSchemaName(msSchemaName);
        msDmsAuditLogDo.setOpTime(opTime);
        msDmsAuditLogDo.setUserName(userName);
        msDmsAuditLogDo.setUserId(userId);
        msDmsAuditLogDo.setInstanceName(instanceName);
        msDmsAuditLogDo.setInstanceId(instanceId);
        msDmsAuditLogDo.setDbId(dbId);
        msDmsAuditLogDo.setLogic(logic);
        msDmsAuditLogDo.setSqlType(sqlType);
        msDmsAuditLogDo.setExecState(execState);
        msDmsAuditLogDo.setAffectRows(affectRows);
        msDmsAuditLogDo.setElapsedTime(elapsedTime);
        msDmsAuditLogDo.setRemark(remark);
        msDmsAuditLogDo.setHash(hash);

        list.add(msDmsAuditLogDo);
      }
      size = list.size();
      if (0 < list.size()) {
        // 将来自DMS的SQL审计日志信息存储到专门存储DMS审计日志的表中；2022-05-30 16:36:22
        msDmsAuditLogDao.insertSelectiveBatchNoSqlInsightDbUserName(list);
      }
    } catch (Exception e) {
      log.error("将阿里云提供的DMS数据库审计日志插入到表中出现了异常。", e);
    }
    log.info("执行完毕# AuditLogServiceImpl.batchProcessDMSAuditLog() # 将阿里云提供的DMS数据库审计日志【{}条】插入到数据库中。耗时【{}】毫秒。", size, DateTimeUtil.getTimeMillis(now));
    return null;
  }

  /**
   * <B>方法名称：getDmsAuditLogFromDb</B>
   * <B>概要说明：从数据库中获取来自DMS的数据库审计日志</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 15:06:35
   * @Param []
   **/
  @Override
  public ServerResponse<String> getDmsAuditLogFromDb(String dbUserName,
                                                     String sqlType, /* SQL语句的类型；是insert、select、update、delete等 */
                                                     String msTableName, /* 数据库表名 */
                                                     String startTime, /* 开始时间 */
                                                     String endTime, /* 结束时间 */
                                                     Integer pageNo,
                                                     Integer pageSize) {
    Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
    if (StringUtil.isNotBlank(sqlType)) {
      queryMap.put("sqlType", sqlType);
    }
    if (StringUtil.isNotBlank(msTableName)) {
      queryMap.put("msTableName", msTableName);
    }
    if (StringUtil.isNotBlank(startTime)) {
      queryMap.put(Const.START_TIME, startTime);
    }
    if (StringUtil.isNotBlank(endTime)) {
      queryMap.put(Const.END_TIME, endTime);
    }
    if (StringUtil.isNotBlank(dbUserName)) {
      queryMap.put("userName", dbUserName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
    queryMap.put(Const.PAGE_SIZE, pageSize);

    List<MsDmsAuditLogDo> listMsAuditLog = msDmsAuditLogDao.selectAll(queryMap);
    Integer count = msDmsAuditLogDao.selectAllCount(queryMap);
    Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
    context.put("rows", JsonUtil.obj2String(listMsAuditLog));
    context.put("total", count);
    log.info("执行完毕 AuditLogServiceImpl.getDmsAuditLogFromDb() # 获取来自DMS的数据库审计日志信息。");
    return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, JsonUtil.obj2String(context));
  }

  @Override
  public ServerResponse<String> getAllUserNameFromDms() {
    List<String> userNameList = msDmsAuditLogDao.selectAllUserName();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(userNameList));
    return bySuccess;
  }

  @Override
  public ServerResponse<String> getAllSqlTypeFromDms() {
    List<String> list = msDmsAuditLogDao.selectAllSqlType();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(list));
    return bySuccess;
  }

  @Override
  public ServerResponse<String> getAllTableNameFromDms() {
    List<String> list = msDmsAuditLogDao.selectAllTableName();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(list));
    return bySuccess;
  }

}
