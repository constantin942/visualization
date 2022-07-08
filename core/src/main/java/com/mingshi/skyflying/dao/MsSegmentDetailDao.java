package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.InstanceTable;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.domain.MsThirdPartyTableListDo;
import com.mingshi.skyflying.domain.UserUsualAndUnusualVisitedData;

import java.util.Date;
import java.util.List;
import java.util.Map;

public interface MsSegmentDetailDao {

  void insertSelectiveBatch(List<MsSegmentDetailDo> list);

  int insertSelective(MsSegmentDetailDo record);

  MsSegmentDetailDo selectByPrimaryKey(Integer id);

  List<MsSegmentDetailDo> selectAllUserNameIsNotNull();

  List<MsSegmentDetailDo> selectAllUserNameIsNotNullAndTableNameIsNotNull();

  List<MsSegmentDetailDo> selectAllUserNameIsNotNullAndVisitedTimeIsZero();

  List<MsSegmentDetailDo> selectAllUserNameIsNotNullAndVisitedTableIsZero();

  List<MsSegmentDetailDo> selectAll(Map<String, Object> map);

  List<Map<String, String>> selectBatchUserNameIsNotNullAndTokeIsNotNull(List<Integer> list);

  List<Map<String, String>> selectAllUserNameIsNotNullAndTokeIsNotNull();

  List<Integer> selectAllId();

  int updateByPrimaryKeySelective(MsSegmentDetailDo record);

  void updateBatch(List<MsSegmentDetailDo> setmentDetailDoList);

  void updateBatchByToken(List<Map<String,String>> list);

  void updateBatchById(List<MsSegmentDetailDo> setmentDetailDoList);

  Long selectCountAll(Map<String, Object> map);

  List<String> selectAllUserName();

  List<String> selectAllMsTableName();

  List<String> selectAllInstanceName();

  List<MsSegmentDetailDo> selectByTokenUserNameGlobalTraceIdIsNotNull();

  List<InstanceTable> selectAllInstanceAndTableName();

  List<MsThirdPartyTableListDo> selectAllInstanceTrueName();

  Long selectCountsOfUser(Map<String, Object> map);


  Long selectCountOfOneUser(Map<String, Object> userName);

  Date selectLastVisitedTime(Map<String, Object> userName);


  List<UserUsualAndUnusualVisitedData> selectUserUsualAndUnusualData(Map<String, Object> queryMap);

  Long selectCountsOfAllRecentSevenDays(Map<String, Object> map);

  Long selectinformationCount();

  Long selectDbInstanceCount();

  Long selectTableCount();

  Long selectUserCount();
}
