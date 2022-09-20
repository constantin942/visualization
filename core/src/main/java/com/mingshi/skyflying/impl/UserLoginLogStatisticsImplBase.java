package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.domain.UserLoginStatistics;
import com.mingshi.skyflying.dao.AiitUserLoginStatisticsMapper;
import com.mingshi.skyflying.service.UserLoginStatisticsService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 *
 *@类描述：
 *@author 49090 2017年12月26日上午11:34:17
 *
 */
@Service("aiitUserLoginStatisticsService")
public class UserLoginLogStatisticsImplBase extends BaseParentServiceImpl<UserLoginStatistics, Long> implements UserLoginStatisticsService {

	@Resource
  AiitUserLoginStatisticsMapper aiitUserLoginStatisticsMapper;

	@Override
	public int insertSelective(UserLoginStatistics record) {
		return aiitUserLoginStatisticsMapper.insertSelective(record);
	}

	@Override
	public UserLoginStatistics selectByPrimaryKey(Integer id) {
		return aiitUserLoginStatisticsMapper.selectByPrimaryKey(id);
	}

	@Override
	public UserLoginStatistics selectPasswordErrorCount(String userName) {
		return aiitUserLoginStatisticsMapper.selectPasswordErrorCount(userName);
	}

	@Override
	public int updateByPrimaryKeySelective(UserLoginStatistics record) {
		return aiitUserLoginStatisticsMapper.updateByPrimaryKeySelective(record);
	}

	@Override
	public int updatePasswordErrorCount(String userName) {
		return aiitUserLoginStatisticsMapper.updatePasswordErrorCount(userName);
	}

}

