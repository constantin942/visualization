package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.DicItem;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmConfig;

import java.util.ArrayList;
import java.util.List;

public interface DingAlarmConfigMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(DingAlarmConfig record);

    int insertSelective(DingAlarmConfig record);

    DingAlarmConfig selectByPrimaryKey(Integer id);

    DingAlarmConfig selectOne();

    int updateByPrimaryKeySelective(DingAlarmConfig record);

    int updateByPrimaryKey(DingAlarmConfig record);

    ArrayList<Integer> selectGapDic();
}