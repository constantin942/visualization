package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.MsMonitorBusinessSystemTablesDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface MsMonitorBusinessSystemTablesService extends ParentService<MsMonitorBusinessSystemTablesDo, Long> {

  /**
   * <B>方法名称：getAllTables</B>
   * <B>概要说明：获取所有的表</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月13日 10:07:54
   * @Param []
   **/
  ServerResponse<String> getAllTables(String tableName, String dbName, String dbAddress,Integer pageNo, Integer pageSize);

  /**
   * <B>方法名称：updateTableInformation</B>
   * <B>概要说明：更新表的启用状态</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月13日 10:07:54
   * @Param []
   **/
  ServerResponse<String> updateTableInformation(Integer tableId, Integer isDelete);

}