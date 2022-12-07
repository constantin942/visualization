package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.anomaly_detection.domain.HighRiskOpt;

import java.util.List;

public interface HighRiskOptMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(HighRiskOpt highRiskOpt);

    int insertSelective(HighRiskOpt highRiskOpt);

    HighRiskOpt selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(HighRiskOpt highRiskOpt);

    int updateByPrimaryKey(HighRiskOpt highRiskOpt);

    List<HighRiskOpt> selectAll();
}
