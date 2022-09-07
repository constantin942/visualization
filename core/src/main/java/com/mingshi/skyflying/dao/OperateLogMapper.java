package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.OperateLog;

import java.util.List;
import java.util.Map;

public interface OperateLogMapper {
    int insertSelective(OperateLog operateLog);

    OperateLog selectByPrimaryKey(Long id);

    int updateByPrimaryKeySelective(OperateLog operateLog);

    OperateLog selectByOrderId(String orderId);

    List<OperateLog> selectAllOperationLog(Map<String,Object> map);
}
