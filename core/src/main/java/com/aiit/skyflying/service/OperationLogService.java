package com.aiit.skyflying.service;

import com.aiit.skyflying.common.domain.OperationLog;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;

/**
 * <B>方法名称：OperationLogService</B>
 * <B>概要说明：获取高危操作的接口</B>
 * @Author zm
 * @Date 2022年09月09日 15:09:50
 * @Param
 * @return
 **/
public interface OperationLogService extends ParentService<OperationLog, Long> {

    ServerResponse<String> getOperationLog(String userName,Integer pageNo,Integer pageSize);

}
