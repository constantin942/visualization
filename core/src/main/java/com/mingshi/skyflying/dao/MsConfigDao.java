package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsConfigDo;

public interface MsConfigDao {
    int insertSelective(MsConfigDo record);

    MsConfigDo selectByPrimaryKey(Integer id);

    MsConfigDo selectByConfigType(String type);

    int updateByPrimaryKeySelective(MsConfigDo record);
}
