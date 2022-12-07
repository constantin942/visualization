package com.aiit.skyflying.dao;

import com.aiit.skyflying.common.domain.UserLoginStatistics;

public interface AiitUserLoginStatisticsMapper {

    int insertSelective(UserLoginStatistics userLoginStatistics);

    UserLoginStatistics selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserLoginStatistics userLoginStatistics);

    UserLoginStatistics selectPasswordErrorCount(String userName);
}
