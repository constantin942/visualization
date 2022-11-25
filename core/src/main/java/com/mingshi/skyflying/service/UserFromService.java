package com.mingshi.skyflying.service;


import com.mingshi.skyflying.common.domain.MsUserFrom;
import com.mingshi.skyflying.common.domain.UserLoginLog;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.service.ParentService;

public interface UserFromService extends ParentService<MsUserFrom, Long> {

	ServerResponse<String> selectAll(Integer pageNo, Integer pageSize);

    ServerResponse<String> updateUserFrom(Integer ruleId, Integer isDelete);

    ServerResponse<String> addUserFrom(String userFromPath, String userFromDesc);
}

