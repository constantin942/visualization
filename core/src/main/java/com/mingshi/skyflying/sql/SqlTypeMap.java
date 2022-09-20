package com.mingshi.skyflying.sql;

import com.mingshi.skyflying.common.constant.Const;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <B>主类名称: SqlTypeMap</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/9/19 16:05
 *
 * @Version 1.0
 **/
public class SqlTypeMap {
  private static final Map<String, SqlType> SQL_TYPE_MAP = new ConcurrentHashMap<>();

  private SqlTypeMap(){}
  
  static {
    SQL_TYPE_MAP.put(Const.SQL_TYPE_SELECT.toLowerCase(), new SelectType());
    SQL_TYPE_MAP.put(Const.SQL_TYPE_INSERT.toLowerCase(), new InsertType());
    SQL_TYPE_MAP.put(Const.SQL_TYPE_UPDATE.toLowerCase(), new UpdateType());
    SQL_TYPE_MAP.put(Const.SQL_TYPE_DELETE.toLowerCase(), new DeleteType());
    SQL_TYPE_MAP.put(Const.SQL_TYPE_CREATE.toLowerCase(), new DeleteType());
  }

  /**
   * 获取表名
   *
   * @param type
   * @param sql
   * @return
   */
  public static List<String> getSqlTable(String type, String sql) {
    List<String> tableList = null;
    SqlType sqlType = SQL_TYPE_MAP.get(type.toLowerCase());
    if (null != sqlType) {
      tableList = sqlType.getSqlType(sql);
    }
    return tableList;
  }
}
