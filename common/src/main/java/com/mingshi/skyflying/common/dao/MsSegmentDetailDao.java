package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.*;

import java.util.List;
import java.util.Map;

public interface MsSegmentDetailDao {

  void insertSelectiveBatch(List<MsSegmentDetailDo> list);

  int insertSelective(MsSegmentDetailDo msSegmentDetailDo);

  MsSegmentDetailDo selectByPrimaryKey(Integer id);

  List<MsSegmentDetailDo> selectAllUserNameIsNotNull();

  List<MsSegmentDetailDo> selectAllUserNameIsNotNullAndTableNameIsNotNull();

  List<MsSegmentDetailDo> selectAllUserNameIsNotNullAndVisitedTimeIsZero();

  List<MsSegmentDetailDo> selectAllUserNameIsNotNullAndVisitedTableIsZero();

  List<MsSegmentDetailDo> selectAllNew(Map<String, Object> map);

  List<MsSegmentDetailDo> selectAllFileOutputAndSendEmail(Map<String, Object> map);

  List<Map<String, String>> selectBatchUserNameIsNotNullAndTokeIsNotNull(List<Integer> list);

  List<Map<String, String>> selectAllUserNameIsNotNullAndTokeIsNotNull();

  List<Integer> selectAllId();

  int updateByPrimaryKeySelective(MsSegmentDetailDo msSegmentDetailDo);

  void updateBatch(List<MsSegmentDetailDo> setmentDetailDoList);

  void updateBatchByToken(List<Map<String, String>> list);

  void updateBatchById(List<MsSegmentDetailDo> setmentDetailDoList);

  Long selectCountAllNew(Map<String, Object> map);

  Long selectCountAllFileOutputAndSendEmail(Map<String, Object> map);

  List<String> selectAllUserName();

  List<String> selectAllMsTableName();

  List<Map<String, String>> selectAllMsTableNameDbInstancePeer();

  List<String> selectAllInstanceName();

  List<MsSegmentDetailDo> selectByTokenUserNameGlobalTraceIdIsNotNull(String startTime);

  List<InstanceTable> selectAllInstanceAndTableName();

  Long selectCountsOfUser(Map<String, Object> map);

  Long selectCountsOfUserByPeerAndDbInstanceAndTableName(Map<String, Object> map);

  Long selectEveryoneEeverydayVisitedTimes(Map<String, Object> map);

  Long selectCountOfOneUser(Map<String, Object> userName);

  Long selectCountOfOneUserByUserName(String userName);

  String selectLastVisitedTime(Map<String, Object> userName);

  String selectLastVisitedTimeByUserName(String userName);

  Map<String, String> selectUserUsualAndUnusualDataByUserName(String userName);

  Long selectCountsOfAllRecentSevenDays(Map<String, Object> map);

  Long selectinformationCount();

  Long selectDbInstanceCount();

  Long selectTableCount();

  List<Map<String,String>> selectTableCountGroupByPeerDbinstanceTableName();

  Long selectUserCount();

  List<String> selectAllTableName();

  Long selectCountOfOneTable(String tableName);

  String selectTableLastVisitedTime(String tableName);

  List<UserUsualAndUnusualVisitedData> selectTableUsualAndUnusualData(String tableName);

  List<AlarmData> selectAlarmData();

  List<UserAlarmData> selectUserAlarmData();

  String selectUserNameByToken(String token);

  String selectUserNameByGlobalTraceId(String globalTraceId);

  int deleteTwoDaysBefore();
}
