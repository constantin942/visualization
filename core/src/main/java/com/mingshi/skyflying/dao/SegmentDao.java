package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.SegmentDo;

import java.util.List;

public interface SegmentDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(SegmentDo record);

    SegmentDo selectByPrimaryKey(Integer id);

    List<SegmentDo> selectByGlobalTraceId(String globalTraceId);

    SegmentDo selectBySegmentId(String segmentId);

    int updateByPrimaryKeySelective(SegmentDo record);

    List<SegmentDo> selectAll();
}
