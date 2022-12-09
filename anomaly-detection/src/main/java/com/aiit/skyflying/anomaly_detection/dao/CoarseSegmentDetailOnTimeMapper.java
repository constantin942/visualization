package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.aiit.skyflying.anomaly_detection.domain.VisitCountOnTimeInterval;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;

public interface CoarseSegmentDetailOnTimeMapper {
    int insertSelectiveBatch(List<CoarseSegmentDetailOnTimeDo> list);

    List<VisitCountOnTimeInterval> selectInfoInPeriod(Integer portraitByTimePeriod);

    List<CoarseSegmentDetailOnTimeDo> selectPeriodInfo();

    CoarseSegmentDetailOnTimeDo selectOneByUsername(String username);

    CoarseSegmentDetailOnTimeDo selectOneByNameAndTime(@Param("username") String username, @Param("time") Date time);

    void insertSelective(CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTimeDo);

    void updateByPrimaryKeySelective(CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTimeDo);
}
