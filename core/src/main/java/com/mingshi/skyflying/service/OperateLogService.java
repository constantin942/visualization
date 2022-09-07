package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.OperateLog;

/**
 * <B>方法名称：OperateLogService</B>
 * <B>概要说明：操作日志接口</B>
 * @Author zm
 * @Date 2022年09月07日 14:09:29
 * @Param
 * @return
 **/
public interface OperateLogService extends ParentService<OperateLog, Long> {

    int insertSelective(OperateLog record);

    OperateLog selectByPrimaryKey(Long id);

    OperateLog selectByOrderId(String orderId);

    int updateByPrimaryKeySelective(OperateLog record);

}
