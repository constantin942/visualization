package com.mingshi.skyflying.service;

import com.mingshi.skyflying.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.response.ServerResponse;

import java.util.List;


public interface MsAlarmInformationService extends ParentService<MsAlarmInformationDo, Long> {
    ServerResponse<String> getAllAlarmInfo(String userName, Integer pageNo, Integer pageSize);

    ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName, Integer matchRuleId, String originalTime, Integer pageNo, Integer pageSize);

    ServerResponse<String> getUserNameAnomalyDetectionInfo();

    ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(Integer pageNo, Integer pageSize);

    /**
     * <B>方法名称：updateAnomalyDetectionInfo</B>
     * <B>概要说明：处置告警异常信息</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年07月25日 13:07:17
     * @Param [userName, matchRuleId, originalTime]
     **/
    ServerResponse<String> updateAnomalyDetectionInfos(List<AnomalyDetectionInfoBo> anomalyDetectionInfoBos);
}
