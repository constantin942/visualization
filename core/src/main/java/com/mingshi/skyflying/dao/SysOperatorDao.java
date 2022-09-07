package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SysOperator;

import java.util.List;
import java.util.Map;

public interface SysOperatorDao {
  int insertSelective(SysOperator record);

  SysOperator selectByPrimaryKey(Integer id);

  int updateByPrimaryKeySelective(SysOperator record);

  SysOperator selectByUserName(String userName);

  List<Map<String,Object>> selectAll();
}
