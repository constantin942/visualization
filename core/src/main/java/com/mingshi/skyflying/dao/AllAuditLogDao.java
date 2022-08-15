package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.AllAuditLogDo;
import com.mingshi.skyflying.common.domain.MsAuditLogDo;

import java.util.List;

public interface AllAuditLogDao {
    int insertSelective(AllAuditLogDo msg);

    MsAuditLogDo selectByPrimaryKey(Integer id);

    void insertSelectiveBatch(List<AllAuditLogDo> list);

    int updateByPrimaryKeySelective(AllAuditLogDo msg);
}
