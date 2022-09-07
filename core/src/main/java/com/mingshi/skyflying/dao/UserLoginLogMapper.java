package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.UserLoginLog;

public interface UserLoginLogMapper {
    int insertSelective(UserLoginLog userLoginLog);

    UserLoginLog selectByPrimaryKey(Long id);

    UserLoginLog selectBySeesionId(String sessionId);

    int updateByPrimaryKeySelective(UserLoginLog record);
}
