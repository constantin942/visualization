package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.HighRiskOpt;

import java.util.List;

public interface HighRiskOptMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(HighRiskOpt record);

    int insertSelective(HighRiskOpt record);

    HighRiskOpt selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(HighRiskOpt record);

    int updateByPrimaryKey(HighRiskOpt record);

    List<HighRiskOpt> selectAll();
}