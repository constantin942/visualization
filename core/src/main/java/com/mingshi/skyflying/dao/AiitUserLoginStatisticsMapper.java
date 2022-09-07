package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.UserLoginStatistics;

public interface AiitUserLoginStatisticsMapper {

    int insertSelective(UserLoginStatistics record);

    UserLoginStatistics selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserLoginStatistics record);

    int updatePasswordErrorCount(String userName);

    UserLoginStatistics selectPasswordErrorCount(String userName);
}
