package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SysMenu;

public interface SysMenuDao {
    int insertSelective(SysMenu record);

    SysMenu selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(SysMenu record);
}
