package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;

import java.util.List;

public interface MsSegmentDetailMapper {

    List<MsSegmentDetailDo> getInfoForCoarseDetail();

}
