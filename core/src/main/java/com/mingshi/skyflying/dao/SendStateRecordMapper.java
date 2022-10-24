package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.SendStateRecord;

import java.util.List;

public interface SendStateRecordMapper {
    int insertSelective(SendStateRecord sendStateRecord);

    SendStateRecord selectByPrimaryKey(Long id);

    void insertSelectiveBatch(List<com.mingshi.skyflying.common.domain.SendStateRecord> list);

    int updateByPrimaryKeySelective(SendStateRecord sendStateRecord);
}
