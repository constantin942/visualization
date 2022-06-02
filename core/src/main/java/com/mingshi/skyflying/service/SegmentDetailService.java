package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.SegmentDetailDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface SegmentDetailService extends ParentService<SegmentDetailDo, Long> {
  ServerResponse<String> getAllSegmentsBySegmentRelation1(String userName, Integer pageNo, Integer pageSize);
  ServerResponse<String> getAllSegmentsBySegmentRelation2(String userName, Integer pageNo, Integer pageSize);
}
