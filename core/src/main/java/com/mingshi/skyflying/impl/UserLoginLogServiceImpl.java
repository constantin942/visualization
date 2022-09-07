package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.domain.UserLoginLog;
import com.mingshi.skyflying.dao.UserLoginLogMapper;
import com.mingshi.skyflying.service.UserLoginLogService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 *
 *@类描述：
 *@author 49090 2017年12月26日上午11:34:17
 *
 */
@Service("aiitUserLoginLogService")
public class UserLoginLogServiceImpl extends ParentServiceImpl<UserLoginLog, Long> implements UserLoginLogService {

	@Resource
  UserLoginLogMapper userLoginLogMapper;

	@Override
	public int insertSelective(UserLoginLog record) {
		return userLoginLogMapper.insertSelective(record);
	}

	@Override
	public UserLoginLog selectByPrimaryKey(Long id) {
		return userLoginLogMapper.selectByPrimaryKey(id);
	}

	@Override
	public UserLoginLog selectBySeesionId(String sessionId) {
		return userLoginLogMapper.selectBySeesionId(sessionId);
	}

	@Override
	public int updateByPrimaryKeySelective(UserLoginLog record) {
		return userLoginLogMapper.updateByPrimaryKeySelective(record);
	}

}

