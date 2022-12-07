package com.aiit.skyflying.service;


import com.aiit.skyflying.common.domain.MsUserFrom;
import com.aiit.skyflying.common.domain.UserLoginLog;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;

public interface UserFromService extends ParentService<MsUserFrom, Long> {

	ServerResponse<String> selectAll(Integer pageNo, Integer pageSize);

    ServerResponse<String> updateUserFrom(Integer ruleId, Integer isDelete);

    ServerResponse<String> addUserFrom(String userFromPath, String userFromDesc);
}

