package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.UserPortraitByVisitedTimeDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitByVisitedTimeMapper {

    int insertSelective(UserPortraitByVisitedTimeDo msg);

    UserPortraitByVisitedTimeDo selectByPrimaryKey(Integer id);

    UserPortraitByVisitedTimeDo selectByRuleId(Integer id);

    List<UserPortraitByVisitedTimeDo> selectAll();

    List<UserPortraitByVisitedTimeDo> selectAllEnable();

    List<UserPortraitByVisitedTimeDo> selectByUserName(Map<String, Object> queryMap);

    Integer selectByUserNameCount(Map<String, Object> queryMap);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTimeDo msg);

    void insertSelectiveBatch(List<UserPortraitByVisitedTimeDo> list);

    void updateBatch(List<UserPortraitByVisitedTimeDo> list);

    List<String> selectAllUserName();
}
