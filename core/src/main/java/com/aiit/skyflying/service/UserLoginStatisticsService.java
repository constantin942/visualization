package com.aiit.skyflying.service;

import com.aiit.skyflying.common.domain.UserLoginStatistics;
import com.aiit.skyflying.common.service.ParentService;

public interface UserLoginStatisticsService extends ParentService<UserLoginStatistics, Long> {

	int insertSelective(UserLoginStatistics userLoginStatistics);

	UserLoginStatistics selectByPrimaryKey(Integer id);

	UserLoginStatistics selectPasswordErrorCount(String userName);

	int updateByPrimaryKeySelective(UserLoginStatistics userLoginStatistics);

}

