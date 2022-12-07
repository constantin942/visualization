package com.aiit.skyflying.common.dao;

import com.aiit.skyflying.common.domain.MsSystemOperationRecord;

import java.util.List;

public interface MsSystemOperationRecordMapper {

    int insertSelective(MsSystemOperationRecord msSystemOperationRecord);

    int insertSelectiveBatch(List<MsSystemOperationRecord> list);

    MsSystemOperationRecord selectByPrimaryKey(Integer id);

    List<MsSystemOperationRecord> selectBySystemName(String systemName);

    int updateByPrimaryKeySelective(MsSystemOperationRecord msSystemOperationRecord);

    int updateBySystemName(MsSystemOperationRecord msSystemOperationRecord);

    int updateBySystemNameAndServiceCode(MsSystemOperationRecord msSystemOperationRecord);

}
