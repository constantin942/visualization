package com.aiit.skyflying.dao;

import com.aiit.skyflying.common.domain.SysMenu;

public interface SysMenuDao {
    int insertSelective(SysMenu sysMenu);

    SysMenu selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(SysMenu sysMenu);
}
