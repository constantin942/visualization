package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserTokenDo;

public interface UserTokenDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(UserTokenDo record);

    UserTokenDo selectByPrimaryKey(Integer id);

    UserTokenDo selectByToken(String token);

    UserTokenDo selectByUserNameAndToken(UserTokenDo userTokenDo);

    int updateByPrimaryKeySelective(UserTokenDo record);
}
