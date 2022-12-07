package com.aiit.skyflying.dao;

import com.aiit.skyflying.domain.SendStateRecord;

import java.util.List;

public interface SendStateRecordMapper {
    int insertSelective(SendStateRecord sendStateRecord);

    SendStateRecord selectByPrimaryKey(Long id);

    void insertSelectiveBatch(List<com.aiit.skyflying.common.domain.SendStateRecord> list);

    int updateByPrimaryKeySelective(SendStateRecord sendStateRecord);
}
