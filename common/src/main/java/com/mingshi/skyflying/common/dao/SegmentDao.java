package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.SegmentDo;

import java.util.List;

public interface SegmentDao {
    int insertSelective(SegmentDo segmentDo);

    void insertSelectiveBatch(List<SegmentDo> list);
}
