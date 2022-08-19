package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SegmentRelationDo;

import java.util.List;
import java.util.Map;

public interface SegmentRelationDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(SegmentRelationDo segmentRelationDo);

    SegmentRelationDo selectByPrimaryKey(Integer id);

    List<Map<String,String>> selectAllGlobalTraceId(Map<String, Object> map);

    SegmentRelationDo selectByGlobalTraceId(Map<String, Object> map);

    int updateByPrimaryKeySelective(SegmentRelationDo segmentRelationDo);
}
