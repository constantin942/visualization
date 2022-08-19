package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsScheduledTaskDo;

public interface MsScheduledTaskDao {
    int insertSelective(MsScheduledTaskDo msScheduledTaskDo);

    MsScheduledTaskDo selectByPrimaryKey(Integer id);

    /**
     * <B>方法名称：selectLastSuccessRecord</B>
     * <B>概要说明：获取上一次执行成功的记录</B>
     * @Author zm
     * @Date 2022年05月26日 17:05:59
     * @Param [id]
     * @return com.mingshi.skyflying.common.domain.MsScheduledTaskDo
     **/
    MsScheduledTaskDo selectLastSuccessRecord(String flag);

    int updateByPrimaryKeySelective(MsScheduledTaskDo msScheduledTaskDo);
}
