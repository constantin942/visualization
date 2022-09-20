package com.mingshi.skyflying.sql;

import java.util.List;

/**
 * <B>接口名称: SqlType</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/9/19 16:04
 *
 * @Version 1.0
 **/
public interface SqlType {
  public List<String> getSqlType(String sql);
}
