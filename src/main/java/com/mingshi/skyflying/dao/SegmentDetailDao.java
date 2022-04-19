package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.SegmentDetailDo;

public interface SegmentDetailDao {

    int insertSelective(SegmentDetailDo record);

    SegmentDetailDo selectByPrimaryKey(String id);

    int updateByPrimaryKeySelective(SegmentDetailDo record);

}
