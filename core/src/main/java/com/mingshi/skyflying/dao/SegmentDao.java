package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.SegmentDo;

import java.util.List;

public interface SegmentDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(SegmentDo segmentDo);

    void insertSelectiveBatch(List<SegmentDo> list);

    SegmentDo selectByPrimaryKey(Integer id);

    List<SegmentDo> selectByGlobalTraceId(String globalTraceId);

    List<SegmentDo> selectByGlobalTraceIdAndUserNameIsNull(String globalTraceId);

    SegmentDo selectBySegmentId(String segmentId);

    int updateByPrimaryKeySelective(SegmentDo segmentDo);

    List<SegmentDo> selectAll();
}
