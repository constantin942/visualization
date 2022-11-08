package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.MsConfigDo;

public interface MsConfigDao {
    int insertSelective(MsConfigDo msConfigDo);

    MsConfigDo selectByConfigType(String type);

    int updateByPrimaryKeySelective(MsConfigDo msConfigDo);
}
