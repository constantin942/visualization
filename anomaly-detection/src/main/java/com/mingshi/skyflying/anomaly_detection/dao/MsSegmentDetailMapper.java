package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;

import java.util.Date;
import java.util.List;
import java.util.Map;

public interface MsSegmentDetailMapper {

    List<MsSegmentDetailDo> getInfoForCoarseDetail();

    MsSegmentDetailDo selectByGlobalTraceId(String traceId);

    Date selectTimeGap(String username);

    List<Map<String, String>> selectFirstVisitTime();
}
