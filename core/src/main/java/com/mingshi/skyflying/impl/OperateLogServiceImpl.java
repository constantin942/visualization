package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.domain.OperateLog;
import com.mingshi.skyflying.dao.OperateLogMapper;
import com.mingshi.skyflying.service.OperateLogService;
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
public class OperateLogServiceImpl extends ParentServiceImpl<OperateLog, Long> implements OperateLogService {

    @Resource
    private OperateLogMapper aiitOperateLogMapper;

    @Override
    public int insertSelective(OperateLog record) {
        return aiitOperateLogMapper.insertSelective(record);
    }

    @Override
    public OperateLog selectByPrimaryKey(Long id) {
        return aiitOperateLogMapper.selectByPrimaryKey(id);
    }

    @Override
    public OperateLog selectByOrderId(String orderId) {
        return aiitOperateLogMapper.selectByOrderId(orderId);
    }

    @Override
    public int updateByPrimaryKeySelective(OperateLog record) {
        return aiitOperateLogMapper.updateByPrimaryKeySelective(record);
    }

}
