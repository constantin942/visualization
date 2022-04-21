package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.Segment;

import java.util.List;

public interface SegmentDao {
    int deleteByPrimaryKey(Integer id);

    int insertSelective(Segment record);

    Segment selectByPrimaryKey(Integer id);

    List<Segment> selectAll();

    int updateByPrimaryKeySelective(Segment record);
}
