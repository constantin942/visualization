package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.AllAuditLogDo;
import com.mingshi.skyflying.domain.MsAuditLogDo;

import java.util.List;

public interface AllAuditLogDao {
    int insertSelective(AllAuditLogDo record);

    MsAuditLogDo selectByPrimaryKey(Integer id);

    void insertSelectiveBatch(List<AllAuditLogDo> list);

    int updateByPrimaryKeySelective(AllAuditLogDo record);
}
