package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.anomaly_detection.domain.DingAlarmInformation;

import java.util.List;

public interface DingAlarmInformationMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(DingAlarmInformation dingAlarmInformation);

    int insertSelective(DingAlarmInformation dingAlarmInformation);

    DingAlarmInformation selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(DingAlarmInformation dingAlarmInformation);

    int updateByPrimaryKey(DingAlarmInformation dingAlarmInformation);

    List<DingAlarmInformation> selectPeriodInfo(Integer dingAlarmPeriod);
}
