package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SysMenuRole;
import java.util.List;

public interface SysMenuRoleDao {
    int insertSelective(SysMenuRole record);

    SysMenuRole selectByPrimaryKey(Long id);

    List<SysMenuRole> selectByRoleId(Integer roleId);

    int updateByPrimaryKeySelective(SysMenuRole record);

		Integer selectReadOnly(Integer roleId);
}
