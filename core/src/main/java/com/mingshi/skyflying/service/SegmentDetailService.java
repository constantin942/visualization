package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.SegmentDetailDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface SegmentDetailService extends ParentService<SegmentDetailDo, Long> {
  ServerResponse<String> getAllSegmentsBySegmentRelation(String userName, Integer pageNo, Integer pageSize);
}
