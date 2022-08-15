package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsAuditLogDo;

import java.util.List;
import java.util.Map;

public interface MsAuditLogDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(MsAuditLogDo record);

    MsAuditLogDo selectByPrimaryKey(Integer id);

    List<MsAuditLogDo> selectByUserName(Map<String, Object> map);

    MsAuditLogDo selectByHash(String hash);

    void insertSelectiveBatch(List<MsAuditLogDo> list);

    void insertSelectiveBatchNoSqlInsightDbUserName(List<MsAuditLogDo> list);

    int updateByPrimaryKeySelective(MsAuditLogDo record);

    int updateBatch(List<MsAuditLogDo> record);

    Long selectCount(Map<String, Object> map);

    List<MsAuditLogDo> selectBehaviorByUserName(Map<String, Object> map);

    List<MsAuditLogDo> selectBehaviorByOptTime(Map<String, Object> map);

    List<MsAuditLogDo> selectBehaviorByTableName(Map<String, Object> queryMap);

    List<String> selectAllUserName();

    List<String> selectAllMsTableName();


}
