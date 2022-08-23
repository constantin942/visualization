package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.MsDmsAuditLogDo;
import com.mingshi.skyflying.common.response.ServerResponse;

public interface AuditLogService extends ParentService<MsDmsAuditLogDo, Long> {

  /**
   * <B>方法名称：autoFetchAuditlogByDMS</B>
   * <B>概要说明：通过定时任务，自动拉取MDS中的数据库审计日志</B>
   * @Author zm
   * @Date 2022年05月26日 15:05:40
   * @Param []
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> autoFetchAuditlogByDms(String startTime,String endTime);

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

  ServerResponse<String> getAllUserNameFromDms();

  ServerResponse<String> getAllSqlTypeFromDms();

  ServerResponse<String> getAllTableNameFromDms();
}
