package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SysOperatorRole;

public interface SysOperatorRoleDao {
    int insertSelective(SysOperatorRole record);

    SysOperatorRole selectByPrimaryKey(Long id);

    SysOperatorRole selectBySysOperatorId(Integer sysOperatorId);

    int updateByPrimaryKeySelective(SysOperatorRole record);
}
