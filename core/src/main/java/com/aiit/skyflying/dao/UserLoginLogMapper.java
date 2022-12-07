package com.aiit.skyflying.dao;

import com.aiit.skyflying.common.domain.UserLoginLog;

import java.util.List;
import java.util.Map;

public interface UserLoginLogMapper {
    int insertSelective(UserLoginLog userLoginLog);

    UserLoginLog selectByPrimaryKey(Long id);

    UserLoginLog selectBySeesionId(String sessionId);

    List<UserLoginLog> selectAll(Map<String, Integer> queryMap);

    int selectAllCount(Map<String, Integer> queryMap);

    int updateByPrimaryKeySelective(UserLoginLog userLoginLog);
}
