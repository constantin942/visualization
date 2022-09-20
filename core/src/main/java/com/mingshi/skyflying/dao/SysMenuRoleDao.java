package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SysMenuRole;
import java.util.List;

public interface SysMenuRoleDao {
    int insertSelective(SysMenuRole sysMenuRole);

    SysMenuRole selectByPrimaryKey(Long id);

    List<SysMenuRole> selectByRoleId(Integer roleId);

    int updateByPrimaryKeySelective(SysMenuRole sysMenuRole);

		Integer selectReadOnly(Integer roleId);
}
