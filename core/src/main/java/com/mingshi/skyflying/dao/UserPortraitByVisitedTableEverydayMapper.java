package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserPortraitByVisitedTableEverydayDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitByVisitedTableEverydayMapper {

    void insertSelectiveBatch(List<UserPortraitByVisitedTableEverydayDo> list);

    int insertSelective(UserPortraitByVisitedTableEverydayDo record);

    UserPortraitByVisitedTableEverydayDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTableEverydayDo record);

    List<UserPortraitByVisitedTableEverydayDo> selectAll();

    List<UserPortraitByVisitedTableEverydayDo> selectAllEnable();

    List<UserPortraitByVisitedTableEverydayDo> selectByUserNameAndVisitedTableAndVisitedDate(Map<String,Object> map);

    Integer selectByUserNameAndVisitedTableAndVisitedDateCount(Map<String,Object> map);

    void updateBatch(List<UserPortraitByVisitedTableEverydayDo> list);

    List<String> selectAllUserName();

    List<String> selectAllVisitedTable();
}
