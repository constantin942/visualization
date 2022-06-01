package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsDmsAuditLogDo;

import java.util.List;

public interface MsDmsAuditLogDao {
    void insertSelectiveBatchNoSqlInsightDbUserName(List<MsDmsAuditLogDo> list);

    int insertSelective(MsDmsAuditLogDo record);

    MsDmsAuditLogDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsDmsAuditLogDo record);

    void insertSelectiveBatch(List<MsDmsAuditLogDo> list);
}
