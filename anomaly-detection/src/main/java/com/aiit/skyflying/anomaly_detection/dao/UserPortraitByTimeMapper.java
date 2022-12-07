package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;

import java.util.List;

public interface UserPortraitByTimeMapper {
    void deleteAll();

    void insertBatch(List<UserPortraitByTimeDo> userPortraitByTimeDoList);
}
