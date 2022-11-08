package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SysOperator;

public interface SysOperatorDao {
  int insertSelective(SysOperator sysOperator);

  SysOperator selectByPrimaryKey(Integer id);

  int updateByPrimaryKeySelective(SysOperator sysOperator);

  SysOperator selectByUserName(String userName);
}
