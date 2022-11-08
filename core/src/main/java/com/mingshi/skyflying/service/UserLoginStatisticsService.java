package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.UserLoginStatistics;
import com.mingshi.skyflying.common.service.ParentService;

public interface UserLoginStatisticsService extends ParentService<UserLoginStatistics, Long> {

	int insertSelective(UserLoginStatistics userLoginStatistics);

	UserLoginStatistics selectByPrimaryKey(Integer id);

	UserLoginStatistics selectPasswordErrorCount(String userName);

	int updateByPrimaryKeySelective(UserLoginStatistics userLoginStatistics);

}

