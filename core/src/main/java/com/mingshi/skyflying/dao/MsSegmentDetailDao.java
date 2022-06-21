package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsSegmentDetailDo;

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

    List<MsSegmentDetailDo> selectAll(Map<String,Object> map);

    int updateByPrimaryKeySelective(MsSegmentDetailDo record);

    void updateBatch(List<MsSegmentDetailDo> setmentDetailDoList);

    void updateBatchById(List<MsSegmentDetailDo> setmentDetailDoList);

    Long selectCountAll(Map<String, Object> map);

    List<String> selectAllUserName();

    List<String> selectAllMsTableName();

    List<String> selectAllInstanceName();

    List<MsSegmentDetailDo> selectByTokenUserNameGlobalTraceIdIsNotNull();
}
