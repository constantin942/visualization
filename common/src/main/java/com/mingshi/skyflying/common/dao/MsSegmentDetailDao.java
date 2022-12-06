package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.*;

import java.util.List;
import java.util.Map;

public interface MsSegmentDetailDao {

  void insertSelectiveBatch(List<MsSegmentDetailDo> list);

  List<MsSegmentDetailDo> selectAllNew(Map<String, Object> map);

  List<MsSegmentDetailDo> selectAllFileOutputAndSendEmail(Map<String, Object> map);

  Long selectCountAllNew(Map<String, Object> map);

  Long selectCountAllFileOutputAndSendEmail(Map<String, Object> map);

  List<String> selectAllUserName();

  List<String> selectAllMsTableName();

  List<MsSegmentDetailDo> selectByTokenUserNameGlobalTraceIdIsNotNull(String startTime);

  List<InstanceTable> selectAllInstanceAndTableName();

  Long selectEveryoneEeverydayVisitedTimes(Map<String, Object> map);

  Long selectCountOfOneUserByUserName(String userName);

  String selectLastVisitedTimeByUserName(String userName);

  Map<String, String> selectUserUsualAndUnusualDataByUserName(String userName);

  Long selectCountsOfAllRecentSevenDays(Map<String, Object> map);

  Long selectCountOfOneTable(String tableName);

  String selectTableLastVisitedTime(String tableName);

  List<AlarmData> selectAlarmDistributionData();

  List<MsAlarmInformationDo> selectAlarmHandledData();

  List<UserAlarmData> selectUserAlarmData();

  String selectUserNameByToken(String token);

  int deleteTwoDaysBefore();
}
