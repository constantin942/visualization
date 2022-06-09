package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTableDo;

import java.util.List;

public interface UserPortraitByVisitedTableMapper {

    int insertSelective(UserPortraitByVisitedTableDo record);

    UserPortraitByVisitedTableDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTableDo record);

    void insertSelectiveBatch(List<UserPortraitByVisitedTableDo> list);

    List<UserPortraitByVisitedTableDo> selectAll();

    void updateBatch(List<UserPortraitByVisitedTableDo> list);
}
