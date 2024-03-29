package com.aiit.skyflying.service;


import com.aiit.skyflying.common.domain.UserLoginLog;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;

/**
 *
 *@类描述：资源服务
 *@author 49090 2017年12月26日上午11:32:46
 *
 */
public interface UserLoginLogService extends ParentService<UserLoginLog, Long> {

	int insertSelective(UserLoginLog userLoginLog);

	UserLoginLog selectByPrimaryKey(Long id);

	ServerResponse<String> selectAll(Integer pageNo, Integer pageSize);

	UserLoginLog selectBySeesionId(String sessionId);

	int updateByPrimaryKeySelective(UserLoginLog userLoginLog);

}

