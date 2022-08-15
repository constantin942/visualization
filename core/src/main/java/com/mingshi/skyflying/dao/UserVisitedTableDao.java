package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.UserVisitedTableDo;

public interface UserVisitedTableDao {
    int deleteByPrimaryKey(Integer id);

    int insert(UserVisitedTableDo record);

    int insertSelective(UserVisitedTableDo record);

    UserVisitedTableDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserVisitedTableDo record);

    int updateByPrimaryKey(UserVisitedTableDo record);
}
