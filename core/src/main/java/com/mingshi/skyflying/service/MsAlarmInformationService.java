package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.response.ServerResponse;


public interface MsAlarmInformationService extends ParentService<MsAlarmInformationDo, Long> {
  ServerResponse<String> getAllAlarmInfo(String userName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName, Integer matchRuleId, String originalTime, Integer pageNo, Integer pageSize);

  ServerResponse<String> getUserNameAnomalyDetectionInfo();

  ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(Integer pageNo, Integer pageSize);

  /**
   * <B>方法名称：updateAnomalyDetectionInfo</B>
   * <B>概要说明：处置告警异常信息</B>
   * @Author zm
   * @Date 2022年07月25日 13:07:17
   * @Param [userName, matchRuleId, originalTime]
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  ServerResponse<String> updateAnomalyDetectionInfo(Integer id, Integer matchRuleId, String originalTime, String userName, String alarmContent, String flag);
}
