package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.SegmentDetailDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface SegmentDetailService extends ParentService<SegmentDetailDo, Long> {
  // ServerResponse<String> getAllSegmentsBySegmentRelation1(String userName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllSegmentsBySegmentRelation2(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserName();

  ServerResponse<String> getAllMsTableName();

  // 2022-06-06 17:02:50
  // 异常检测规则有：
  // 1. 若某用户通常白天访问数据，则夜间为异常；
  // 2. 某用户访问从未访问过的表；
  // 3. 访问频率、访问量激增；
}
