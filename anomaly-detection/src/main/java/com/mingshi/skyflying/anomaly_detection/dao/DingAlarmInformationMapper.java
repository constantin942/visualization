package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmInformation;

import java.util.List;

public interface DingAlarmInformationMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(DingAlarmInformation record);

    int insertSelective(DingAlarmInformation record);

    DingAlarmInformation selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(DingAlarmInformation record);

    int updateByPrimaryKey(DingAlarmInformation record);

    List<DingAlarmInformation> selectPeriodInfo(Integer dingAlarmPeriod);
}