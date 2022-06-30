package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface MsAlarmInformationService extends ParentService<MsAlarmInformationDo, Long> {
  ServerResponse<String> getAllAlarmInfo(String userName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName, Integer matchRuleId, String originalTime, Integer pageNo, Integer pageSize);

  ServerResponse<String> getUserNameAnomalyDetectionInfo();

  ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(Integer pageNo, Integer pageSize);
}
