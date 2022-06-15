package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsDmsAuditLogDo;

import java.util.List;
import java.util.Map;

public interface MsDmsAuditLogDao {
    void insertSelectiveBatchNoSqlInsightDbUserName(List<MsDmsAuditLogDo> list);

    int insertSelective(MsDmsAuditLogDo record);

    MsDmsAuditLogDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsDmsAuditLogDo record);

    void insertSelectiveBatch(List<MsDmsAuditLogDo> list);

		List<MsDmsAuditLogDo> selectAll(Map<String, Object> queryMap);

		Integer selectAllCount(Map<String, Object> queryMap);

    List<String> selectAllUserName();

    List<String> selectAllSqlType();
  
    List<String> selectAllTableName();
}
