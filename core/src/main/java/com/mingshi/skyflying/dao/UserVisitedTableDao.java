package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.UserVisitedTableDo;

public interface UserVisitedTableDao {
    int deleteByPrimaryKey(Integer id);

    int insert(UserVisitedTableDo userVisitedTableDo);

    int insertSelective(UserVisitedTableDo userVisitedTableDo);

    UserVisitedTableDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserVisitedTableDo userVisitedTableDo);

    int updateByPrimaryKey(UserVisitedTableDo userVisitedTableDo);
}
