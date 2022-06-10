package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTableEverydayDo;

import java.util.List;

public interface UserPortraitByVisitedTableEverydayMapper {

    void insertSelectiveBatch(List<UserPortraitByVisitedTableEverydayDo> list);

    int insertSelective(UserPortraitByVisitedTableEverydayDo record);

    UserPortraitByVisitedTableEverydayDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTableEverydayDo record);

    List<UserPortraitByVisitedTableEverydayDo> selectAll();

    void updateBatch(List<UserPortraitByVisitedTableEverydayDo> list);
}
