package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;

import java.util.List;

public interface UserPortraitByTableMapper {
    void insertBatch(List<UserPortraitByTableDo> userPortraitByTableDoList);

    List<UserPortraitByTableDo> selectPeriodInfo();
}
