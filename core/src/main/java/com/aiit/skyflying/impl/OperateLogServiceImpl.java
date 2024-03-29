package com.aiit.skyflying.impl;

import com.aiit.skyflying.common.domain.OperationLog;
import com.aiit.skyflying.dao.OperateLogMapper;
import com.aiit.skyflying.service.OperateLogService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <B>方法名称：OperateLogServiceImpl</B>
 * <B>概要说明：操作日志实现类</B>
 * @Author zm
 * @Date 2022年09月07日 14:09:56
 * @Param
 * @return
 **/
@Service("operateLogService")
public class OperateLogServiceImpl extends BaseParentServiceImpl<OperationLog, Long> implements OperateLogService {

    @Resource
    private OperateLogMapper aiitOperateLogMapper;

    @Override
    public int insertSelective(OperationLog operationLog) {
        return aiitOperateLogMapper.insertSelective(operationLog);
    }

    @Override
    public OperationLog selectByPrimaryKey(Long id) {
        return aiitOperateLogMapper.selectByPrimaryKey(id);
    }

    @Override
    public OperationLog selectByOrderId(String orderId) {
        return aiitOperateLogMapper.selectByOrderId(orderId);
    }

    @Override
    public int updateByPrimaryKeySelective(OperationLog operationLog) {
        return aiitOperateLogMapper.updateByPrimaryKeySelective(operationLog);
    }

}
