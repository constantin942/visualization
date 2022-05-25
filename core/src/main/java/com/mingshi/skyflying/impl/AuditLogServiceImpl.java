package com.mingshi.skyflying.impl;


import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.mingshi.skyflying.dao.MsAuditLogDao;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.DateTimeUtil;
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

  @Override
  public ServerResponse<String> process(List<ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog> listSQLExecAuditLogList) {
    int size = 0;
    Instant now = Instant.now();
    log.info("开始执行# AuditLogServiceImpl.process() # 将阿里云提供的数据库审计日志插入到数据库中。");
    try {
      List<MsAuditLogDo> list = new LinkedList<>();
      for (ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog dmsAuditLogDo : listSQLExecAuditLogList) {
        String schemaName = dmsAuditLogDo.getSchemaName();
        if(schemaName.equals("mysql")){
          continue;
        }
        String sql = dmsAuditLogDo.getSQL();
        if(sql.startsWith("show variables") || sql.startsWith("set global") || sql.startsWith("select @@version")){
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
}
