package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;

public interface UserPortraitByTimeMapper {
    int deleteByPrimaryKey(Integer id);

    int insert(UserPortraitByTimeDo record);

    int insertSelective(UserPortraitByTimeDo record);

    UserPortraitByTimeDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitByTimeDo record);

    int updateByPrimaryKey(UserPortraitByTimeDo record);
}