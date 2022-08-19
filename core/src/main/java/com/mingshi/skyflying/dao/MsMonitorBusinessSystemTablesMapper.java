package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsMonitorBusinessSystemTablesDo;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public interface MsMonitorBusinessSystemTablesMapper {

    int insertSelective(MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo);

    MsMonitorBusinessSystemTablesDo selectByPrimaryKey(Integer id);

    MsMonitorBusinessSystemTablesDo selectByTableName(String tableName);

    List<MsMonitorBusinessSystemTablesDo> selectAll();

    List<MsMonitorBusinessSystemTablesDo> selectAllByQueryMap(Map<String, Object> queryMap);

    MsMonitorBusinessSystemTablesDo selectByQueryMap(Map<String, Object> queryMap);

    Integer selectAllByQueryMapCount(Map<String, Object> queryMap);

    List<MsMonitorBusinessSystemTablesDo> selectAllEnable(Map<String, Object> queryMap);

    Integer selectAllEnableDbCount();

    Integer selectAllEnableTableCount();

    Integer selectAllEnableCount(Map<String, Object> queryMap);

    List<MsMonitorBusinessSystemTablesDo> selectAllNotEnable();

    int updateByPrimaryKeySelective(MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo);

    void insertSelectiveBatch(LinkedList<MsMonitorBusinessSystemTablesDo> list);

    Long selectAllDbName();

    Long selectAllTables();
}
