package com.aiit.skyflying.common.dao;

import com.aiit.skyflying.common.domain.MsAlarmInformationDo;

import java.util.List;
import java.util.Map;

public interface MsAlarmInformationMapper {

    int updateByPrimaryKeySelective(MsAlarmInformationDo msAlarmInformationDo);

    void insertSelectiveBatch(List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist);

    List<MsAlarmInformationDo> selectAll(Map<String, Object> queryMap);

    List<Map<String, Object>> selectAllUserTimes(Map<String, Object> queryMap);

    List<Map<String, Object>> selectAllByGroupByUserName(Map<String, Object> queryMap);

    Integer selectAllUserTimesCount(Map<String, Object> queryMap);

    Integer selectAllCount(Map<String, Object> queryMap);

    List<String> selectAllUserName();

    Integer selectAllByGroupByUserNameCount();

    MsAlarmInformationDo selectByGlobalTraceId(String traceId);
}
