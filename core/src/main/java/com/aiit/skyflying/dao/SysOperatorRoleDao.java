package com.aiit.skyflying.dao;

import com.aiit.skyflying.common.domain.SysOperatorRole;

public interface SysOperatorRoleDao {
    int insertSelective(SysOperatorRole sysOperatorRole);

    SysOperatorRole selectByPrimaryKey(Long id);

    SysOperatorRole selectBySysOperatorId(Integer sysOperatorId);

    int updateByPrimaryKeySelective(SysOperatorRole sysOperatorRole);
}
