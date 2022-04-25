package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.SegmentRelationDo;

import java.util.List;

public interface SegmentRelationDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(SegmentRelationDo record);

    SegmentRelationDo selectByPrimaryKey(Integer id);

    List<String> selectAllGlobalTraceId();

    SegmentRelationDo selectByGlobalTraceId(String globalTraceId);

    int updateByPrimaryKeySelective(SegmentRelationDo record);
}
