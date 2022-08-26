package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;

public interface CoarseSegmentDetailOnTimeMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(CoarseSegmentDetailOnTimeDo record);

    int insertSelective(CoarseSegmentDetailOnTimeDo record);

    CoarseSegmentDetailOnTimeDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(CoarseSegmentDetailOnTimeDo record);

    int updateByPrimaryKey(CoarseSegmentDetailOnTimeDo record);
}