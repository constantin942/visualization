package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.SegmentDetailDo;

import java.util.List;

public interface SegmentDetailDao {

    int insertSelective(SegmentDetailDo record);

    SegmentDetailDo selectByPrimaryKey(String id);

    List<SegmentDetailDo> selectAll();

    int updateByPrimaryKeySelective(SegmentDetailDo record);

}
