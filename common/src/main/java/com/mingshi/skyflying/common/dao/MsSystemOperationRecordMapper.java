package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.MsSystemOperationRecord;

public interface MsSystemOperationRecordMapper {

    int insertSelective(MsSystemOperationRecord msSystemOperationRecord);

    MsSystemOperationRecord selectByPrimaryKey(Integer id);

    MsSystemOperationRecord selectBySystemName(String systemName);

    int updateByPrimaryKeySelective(MsSystemOperationRecord msSystemOperationRecord);

    int updateBySystemName(MsSystemOperationRecord msSystemOperationRecord);

}
