package com.mingshi.skyflying.service;


import com.mingshi.skyflying.common.domain.UserLoginLog;

/**
 *
 *@类描述：资源服务
 *@author 49090 2017年12月26日上午11:32:46
 *
 */
public interface UserLoginLogService extends ParentService<UserLoginLog, Long> {

	int insertSelective(UserLoginLog userLoginLog);

	UserLoginLog selectByPrimaryKey(Long id);

	UserLoginLog selectBySeesionId(String sessionId);

	int updateByPrimaryKeySelective(UserLoginLog record);

}

