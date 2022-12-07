package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.anomaly_detection.domain.DingAlarmConfig;

import java.util.ArrayList;

public interface DingAlarmConfigMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(DingAlarmConfig dingAlarmConfig);

    int insertSelective(DingAlarmConfig dingAlarmConfig);

    DingAlarmConfig selectByPrimaryKey(Integer id);

    DingAlarmConfig selectOne();

    int updateByPrimaryKeySelective(DingAlarmConfig dingAlarmConfig);

    int updateByPrimaryKey(DingAlarmConfig dingAlarmConfig);

    ArrayList<Integer> selectGapDic();
}
