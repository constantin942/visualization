package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsAuditLogDo;

import java.util.List;

public interface MsAuditLogDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(MsAuditLogDo record);

    MsAuditLogDo selectByPrimaryKey(Integer id);

    MsAuditLogDo selectByHash(String hash);

    void insertSelectiveBatch(List<MsAuditLogDo> list);

    void insertSelectiveBatchNoSqlInsightDbUserName(List<MsAuditLogDo> list);

    int updateByPrimaryKeySelective(MsAuditLogDo record);

    int updateBatch(List<MsAuditLogDo> record);
}
