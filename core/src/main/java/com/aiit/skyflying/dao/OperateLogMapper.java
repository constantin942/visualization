package com.aiit.skyflying.dao;

import com.aiit.skyflying.common.domain.OperationLog;

import java.util.List;
import java.util.Map;

public interface OperateLogMapper {
    int insertSelective(OperationLog operationLog);

    OperationLog selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(OperationLog operationLog);

    OperationLog selectByOrderId(String orderId);

    List<OperationLog> selectAllOperationLog(Map<String,Object> map);

    Integer selectAllOperationLogCount(Map<String,Object> map);
}
