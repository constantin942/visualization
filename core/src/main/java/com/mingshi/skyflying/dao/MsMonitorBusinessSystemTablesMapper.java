package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsMonitorBusinessSystemTablesDo;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public interface MsMonitorBusinessSystemTablesMapper {

    int insertSelective(MsMonitorBusinessSystemTablesDo record);

    MsMonitorBusinessSystemTablesDo selectByPrimaryKey(Integer id);

    List<MsMonitorBusinessSystemTablesDo> selectAll();

    List<MsMonitorBusinessSystemTablesDo> selectAllByQueryMap(Map<String, Object> queryMap);

    Integer selectAllByQueryMapCount(Map<String, Object> queryMap);

    List<MsMonitorBusinessSystemTablesDo> selectAllEnable();

    List<MsMonitorBusinessSystemTablesDo> selectAllNotEnable();

    int updateByPrimaryKeySelective(MsMonitorBusinessSystemTablesDo record);

    void insertSelectiveBatch(LinkedList<MsMonitorBusinessSystemTablesDo> list);
}
