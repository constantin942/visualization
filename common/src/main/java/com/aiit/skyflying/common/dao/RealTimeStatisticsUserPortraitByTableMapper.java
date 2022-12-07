package com.aiit.skyflying.common.dao;

import com.aiit.skyflying.common.domain.RealTimeStatisticsUserPortraitByTable;
import com.aiit.skyflying.common.domain.UserCoarseInfo;

import java.util.List;
import java.util.Map;

public interface RealTimeStatisticsUserPortraitByTableMapper {
    int insertSelective(RealTimeStatisticsUserPortraitByTable record);

    RealTimeStatisticsUserPortraitByTable selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(RealTimeStatisticsUserPortraitByTable record);

    int insertSelectiveBatch(List<RealTimeStatisticsUserPortraitByTable> coarseInfoList);

    List<UserCoarseInfo> selectAll(Map<String, Object> queryMap);

    Integer selectAllCount(Map<String, Object> queryMap);

    int updateByUserNamAndUsualVisitedData(RealTimeStatisticsUserPortraitByTable realTimeStatisticsUserPortraitByTable);
}
