package com.mingshi.skyflying.service;

import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.response.ServerResponse;

import java.util.List;

public interface AuditLogService extends ParentService<MsAuditLogDo, Long> {
  ServerResponse<String> process(List<ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog> listSQLExecAuditLogList);



  /**
   * <B>方法名称：autoFetchAuditlogByDMS</B>
   * <B>概要说明：通过定时任务，自动拉取MDS中的数据库审计日志</B>
   * @Author zm
   * @Date 2022年05月26日 15:05:40
   * @Param []
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> autoFetchAuditlogByDMS(String startTime,String endTime);

  /**
   * <B>方法名称：getAuditlogByExcel</B>
   * <B>概要说明：从excel表中读取审计日志</B>
   * @Author zm
   * @Date 2022年05月26日 19:05:57
   * @Param []
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> getAuditlogByExcel(String path);

  ServerResponse<String> getBehaviorByUserName(String applicationUserName, String opTime, Integer pageNo, Integer pageSize);

  ServerResponse<String> getBehaviorByOptTime(String sqlInsightDbUserName,String startTime,String endTime,Integer pageNo, Integer pageSize);

  ServerResponse<String> getBehaviorByTableName(String msTableName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserName();

  ServerResponse<String> getAllMsTableName();

  ServerResponse<String> getNumberOfTablesByOpTime(String msTableName, String startTime, String endTime, Integer pageNo, Integer pageSize);


}
