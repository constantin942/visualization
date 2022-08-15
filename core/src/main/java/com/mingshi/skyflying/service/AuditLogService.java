package com.mingshi.skyflying.service;

import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.mingshi.skyflying.common.domain.MsAuditLogDo;
import com.mingshi.skyflying.common.response.ServerResponse;

import java.util.List;

public interface AuditLogService extends ParentService<MsAuditLogDo, Long> {
  ServerResponse<String> process(List<ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog> listSQLExecAuditLogList);



  /**
   * <B>方法名称：autoFetchAuditlogByDMS</B>
   * <B>概要说明：通过定时任务，自动拉取MDS中的数据库审计日志</B>
   * @Author zm
   * @Date 2022年05月26日 15:05:40
   * @Param []
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> autoFetchAuditlogByDMS(String startTime,String endTime);

  /**
   * <B>方法名称：getAuditlogByExcel</B>
   * <B>概要说明：从excel表中读取审计日志</B>
   * @Author zm
   * @Date 2022年05月26日 19:05:57
   * @Param []
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> getAuditlogByExcel(String path);

  ServerResponse<String> getBehaviorByUserName(String applicationUserName, String opTime, Integer pageNo, Integer pageSize);

  ServerResponse<String> getBehaviorByOptTime(String sqlInsightDbUserName,String startTime,String endTime,Integer pageNo, Integer pageSize);

  ServerResponse<String> getBehaviorByTableName(String msTableName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserName();

  ServerResponse<String> getAllMsTableName();

  ServerResponse<String> getNumberOfTablesByOpTime(String msTableName, String startTime, String endTime, Integer pageNo, Integer pageSize);

  /**
   * <B>方法名称：getDmsAuditLogFromDb</B>
   * <B>概要说明：从数据库中获取来自DMS的数据库审计日志</B>
   * @Author zm
   * @Date 2022年06月15日 15:06:35
   * @Param []
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> getDmsAuditLogFromDb(String dbUserName,
                                              String dbType, /* SQL语句的类型；是insert、select、update、delete等 */
                                              String msTableName, /* 数据库表名 */
                                              String startTime, /* 开始时间 */
                                              String endTime, /* 结束时间 */
                                              Integer pageNo,
                                              Integer pageSize);

  ServerResponse<String> getAllUserNameFromDMS();

  ServerResponse<String> getAllSqlTypeFromDMS();

  ServerResponse<String> getAllTableNameFromDMS();
}
