package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;

import java.util.List;
import java.util.Map;

public interface MsAlarmInformationMapper {

    int insertSelective(MsAlarmInformationDo msAlarmInformationDo);

    MsAlarmInformationDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsAlarmInformationDo msAlarmInformationDo);

    int updateByUserNameAndOriginalTime(MsAlarmInformationDo msAlarmInformationDo);

    void insertSelectiveBatch(List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist);

		List<MsAlarmInformationDo> selectAll(Map<String, Object> queryMap);

		List<Map<String,Object>> selectAllUserTimes(Map<String, Object> queryMap);

		List<Map<String,Object>> selectAllByGroupByUserName(Map<String, Object> queryMap);

    Integer selectAllUserTimesCount(Map<String, Object> queryMap);

    Integer selectAllCount(Map<String, Object> queryMap);

    List<String> selectAllUserName();

    Integer selectAllByGroupByUserNameCount();
}
