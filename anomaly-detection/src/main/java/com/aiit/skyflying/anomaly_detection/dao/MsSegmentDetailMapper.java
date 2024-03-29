package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.common.domain.MsSegmentDetailDo;

import java.util.Date;
import java.util.List;
import java.util.Map;

public interface MsSegmentDetailMapper {

    List<MsSegmentDetailDo> getInfoForCoarseDetail();

    List<MsSegmentDetailDo> getInfoForCoarseDetailByPage(Map<String, Object> queryMap);

    List<MsSegmentDetailDo> getInfoForCoarseDetailByStartTime(String startTime);

    List<String> getAllStartTime();

    MsSegmentDetailDo selectByGlobalTraceId(String traceId);

    Date selectTimeGap(String username);

    List<Map<String, String>> selectFirstVisitTime();

    Long selectCountAll();
}
