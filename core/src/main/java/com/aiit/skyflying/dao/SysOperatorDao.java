package com.aiit.skyflying.dao;

import com.aiit.skyflying.common.domain.SysOperator;

public interface SysOperatorDao {
  int insertSelective(SysOperator sysOperator);

  SysOperator selectByPrimaryKey(Integer id);

  int updateByPrimaryKeySelective(SysOperator sysOperator);

  SysOperator selectByUserName(String userName);
}
