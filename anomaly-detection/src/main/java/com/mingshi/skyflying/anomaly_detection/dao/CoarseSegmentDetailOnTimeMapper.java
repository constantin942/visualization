package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;

import java.util.List;

public interface CoarseSegmentDetailOnTimeMapper {
    void insertSelectiveBatch(List<CoarseSegmentDetailOnTimeDo> list);
}