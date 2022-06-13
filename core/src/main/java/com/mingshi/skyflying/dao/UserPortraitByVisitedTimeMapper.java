package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitByVisitedTimeMapper {

    int insertSelective(UserPortraitByVisitedTimeDo record);

    UserPortraitByVisitedTimeDo selectByPrimaryKey(Integer id);

    List<UserPortraitByVisitedTimeDo> selectAll();

    List<UserPortraitByVisitedTimeDo> selectByUserName(Map<String, Object> queryMap);

    Integer selectByUserNameCount(Map<String, Object> queryMap);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTimeDo record);

    void insertSelectiveBatch(List<UserPortraitByVisitedTimeDo> list);

    void updateBatch(List<UserPortraitByVisitedTimeDo> list);

    List<String> selectAllUserName();
}
