package com.aiit.skyflying.common.sql;

import com.aiit.skyflying.common.utils.SqlParserUtils;

import java.util.List;

/**
 * <B>主类名称: SelectType</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/9/19 16:04
 *
 * @Version 1.0
 **/
public class InsertType implements SqlType{
  @Override
  public List<String> getSqlType(String sql) {
    return SqlParserUtils.insertTable(sql);
  }
}
