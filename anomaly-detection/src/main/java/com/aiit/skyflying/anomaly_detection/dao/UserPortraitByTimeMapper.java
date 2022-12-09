package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;

import java.util.List;

public interface UserPortraitByTimeMapper {
    int deleteAll();

    int insertBatch(List<UserPortraitByTimeDo> userPortraitByTimeDoList);
}
