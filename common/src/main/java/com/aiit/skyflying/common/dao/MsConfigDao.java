package com.aiit.skyflying.common.dao;

import com.aiit.skyflying.common.domain.MsConfigDo;

public interface MsConfigDao {
    int insertSelective(MsConfigDo msConfigDo);

    MsConfigDo selectByConfigType(String type);

    int updateByPrimaryKeySelective(MsConfigDo msConfigDo);
}
