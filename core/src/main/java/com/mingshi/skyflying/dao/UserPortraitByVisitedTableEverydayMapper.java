package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.InstanceTable;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTableEverydayDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitByVisitedTableEverydayMapper {

    void insertSelectiveBatch(List<UserPortraitByVisitedTableEverydayDo> list);

    int insertSelective(UserPortraitByVisitedTableEverydayDo msg);

    UserPortraitByVisitedTableEverydayDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitByVisitedTableEverydayDo msg);

    List<UserPortraitByVisitedTableEverydayDo> selectAll();

    List<UserPortraitByVisitedTableEverydayDo> selectAllEnable(Map<String,Integer> query);

    List<UserPortraitByVisitedTableEverydayDo> selectByUserNameAndVisitedTableAndVisitedDate(Map<String,Object> map);

    Integer selectByUserNameAndVisitedTableAndVisitedDateCount(Map<String,Object> map);

    void updateBatch(List<UserPortraitByVisitedTableEverydayDo> list);

    List<String> selectAllUserName();

    List<InstanceTable> selectAllVisitedTable();

    UserPortraitByVisitedTableEverydayDo selectByUserNameAndTime(UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo);
}
