package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsAlarmInformationDo;

import java.util.List;
import java.util.Map;

public interface MsAlarmInformationMapper {

    int insertSelective(MsAlarmInformationDo record);

    MsAlarmInformationDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsAlarmInformationDo record);

    void insertSelectiveBatch(List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist);

		List<MsAlarmInformationDo> selectAll(Map<String, Object> queryMap);

    Integer selectAllCount(Map<String, Object> queryMap);

    List<String> selectAllUserName();
}
