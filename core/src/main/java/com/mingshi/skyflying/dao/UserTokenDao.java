package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserTokenDo;

import java.util.List;
import java.util.Map;

public interface UserTokenDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(UserTokenDo record);

    UserTokenDo selectByPrimaryKey(Integer id);

    UserTokenDo selectByToken(String token);

    UserTokenDo selectByGlobalTraceId(String globalTraceId);

    UserTokenDo selectByGlobalTraceIdUserNameIsNotNull(String globalTraceId);

    List<UserTokenDo> selectByGlobalTraceIdUserNameIsNull(String globalTraceId);

    List<UserTokenDo> selectByTokenUserNameIsNull(String globalTraceId);

    List<UserTokenDo> selectByTokenUserNameGlobalTraceIdIsNotNull();

    UserTokenDo selectByUserNameAndToken(UserTokenDo userTokenDo);

    UserTokenDo selectByUserNameAndTokenAndGlobalTraceId(UserTokenDo userTokenDo);

    Integer insertSelectiveBatch(List<UserTokenDo> userTokenDo);

    List<UserTokenDo> selectByUserName(Map<String, Object> map);

    int updateByPrimaryKeySelective(UserTokenDo record);
}
