package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.UserLoginStatistics;

/**
 *
 *@类描述：资源服务
 *@author 49090 2017年12月26日上午11:32:46
 *
 */
public interface UserLoginStatisticsService extends ParentService<UserLoginStatistics, Long> {

	int insertSelective(UserLoginStatistics userLoginStatistics);

	UserLoginStatistics selectByPrimaryKey(Integer id);

	UserLoginStatistics selectPasswordErrorCount(String userName);

	int updateByPrimaryKeySelective(UserLoginStatistics userLoginStatistics);

	int updatePasswordErrorCount(String userName);

}

