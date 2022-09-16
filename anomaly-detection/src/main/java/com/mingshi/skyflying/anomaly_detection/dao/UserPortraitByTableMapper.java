package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import org.apache.ibatis.annotations.Param;

import java.util.Date;
import java.util.List;

public interface UserPortraitByTableMapper {
    void insertBatch(List<UserPortraitByTableDo> userPortraitByTableDoList);

    void insertOne(UserPortraitByTableDo userPortraitByTableDoList);

    List<UserPortraitByTableDo> selectPeriodInfo(Integer portraitByTablePeriod);

    UserPortraitByTableDo selectByNameAndTime(@Param("username") String username, @Param("time") Date time,
                                              @Param("tableName") String tableName);

    void updateByPrimaryKeySelective(UserPortraitByTableDo userPortraitByTable);
}
