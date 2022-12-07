package com.aiit.skyflying.common.dao;

import com.aiit.skyflying.common.domain.MsMonitorBusinessSystemTablesDo;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public interface MsMonitorBusinessSystemTablesMapper {

    MsMonitorBusinessSystemTablesDo selectByPrimaryKey(Integer id);

    List<MsMonitorBusinessSystemTablesDo> selectAll();

    List<MsMonitorBusinessSystemTablesDo> selectAllByQueryMap(Map<String, Object> queryMap);

    MsMonitorBusinessSystemTablesDo selectByQueryMap(Map<String, Object> queryMap);

    Integer selectAllByQueryMapCount(Map<String, Object> queryMap);

    List<MsMonitorBusinessSystemTablesDo> selectAllEnable(Map<String, Object> queryMap);

    Integer selectAllEnableDbCount();

    Integer selectAllEnableTableCount();

    Integer selectAllEnableCount(Map<String, Object> queryMap);

    int updateByPrimaryKeySelective(MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo);

    void insertSelectiveBatch(LinkedList<MsMonitorBusinessSystemTablesDo> list);

}
