package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.common.domain.UserCoarseInfo;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;
import java.util.Map;

public interface UserPortraitByTableMapper {
    void insertBatch(List<UserPortraitByTableDo> userPortraitByTableDoList);

    void insertOne(UserPortraitByTableDo userPortraitByTableDoList);

    List<UserPortraitByTableDo> selectPeriodInfo(Integer portraitByTablePeriod);

    UserPortraitByTableDo selectByNameAndTime(@Param("username") String username, @Param("time") Date time,
                                              @Param("tableName") String tableName);

    void updateByPrimaryKeySelective(UserPortraitByTableDo userPortraitByTable);

    List<Map<String, String>> selectFrequntList(@Param("username") String userName, @Param("period") Integer ruleTablePeriod
            , @Param("threshold") Integer threshold);

    List<Map<String, String>> selectUnFrequntList(@Param("username") String userName, @Param("period") Integer ruleTablePeriod
            , @Param("threshold") Integer threshold);

    UserCoarseInfo selectCoarseCountsOfUser(@Param("username") String username, @Param("period") Integer period);

    List<String> getAllUser(Map<String, Object> queryMap);

    List<String> getAllUserByOrder(Map<String, Object> queryMap);

    Integer getAllUserCount(Map<String, Object> queryMap);

    String getLastVisitedDate(String username);

    Long getCounts(String user);
}
