package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;

import java.util.List;

public interface UserPortraitByVisitedTimeMapper {

    int insertSelective(UserPortraitByVisitedTimeDo record);

    UserPortraitByVisitedTimeDo selectByPrimaryKey(Integer id);

    List<UserPortraitByVisitedTimeDo> selectAll();

    int updateByPrimaryKeySelective(UserPortraitByVisitedTimeDo record);

    void insertSelectiveBatch(List<UserPortraitByVisitedTimeDo> list);

    void updateBatch(List<UserPortraitByVisitedTimeDo> list);

}
