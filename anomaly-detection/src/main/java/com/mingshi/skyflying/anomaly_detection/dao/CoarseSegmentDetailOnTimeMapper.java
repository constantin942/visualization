package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.VisitCountOnTimeInterval;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;
import java.util.Map;

public interface CoarseSegmentDetailOnTimeMapper {
    void insertSelectiveBatch(List<CoarseSegmentDetailOnTimeDo> list);

    List<VisitCountOnTimeInterval> selectInfoInPeriod(Integer portraitByTimePeriod);

    CoarseSegmentDetailOnTimeDo selectOneByUsername(String username);

    CoarseSegmentDetailOnTimeDo selectOneByNameAndTime(@Param("username") String username, @Param("time") Date time);

    void insertSelective(CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTimeDo);

    void updateByPrimaryKeySelective(CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTimeDo);
}