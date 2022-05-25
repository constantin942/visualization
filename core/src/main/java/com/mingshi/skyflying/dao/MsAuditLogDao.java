package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsAuditLogDo;

import java.util.List;

public interface MsAuditLogDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(MsAuditLogDo record);

    MsAuditLogDo selectByPrimaryKey(Integer id);

    void insertSelectiveBatch(List<MsAuditLogDo> list);

    int updateByPrimaryKeySelective(MsAuditLogDo record);

}
