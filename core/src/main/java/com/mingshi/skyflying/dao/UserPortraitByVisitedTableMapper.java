package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.UserPortraitByVisitedTableDo;

import java.util.List;

public interface UserPortraitByVisitedTableMapper {

    int insertSelective(UserPortraitByVisitedTableDo msg);

    UserPortraitByVisitedTableDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTableDo msg);

    void insertSelectiveBatch(List<UserPortraitByVisitedTableDo> list);

    List<UserPortraitByVisitedTableDo> selectAll();

    void updateBatch(List<UserPortraitByVisitedTableDo> list);
}
