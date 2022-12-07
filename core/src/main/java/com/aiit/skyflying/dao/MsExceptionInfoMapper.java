package com.aiit.skyflying.dao;

import com.aiit.skyflying.domain.MsExceptionInfo;

import java.util.List;

public interface MsExceptionInfoMapper {

    int insertSelective(MsExceptionInfo msExceptionInfo);

    int insertSelectiveBatch(List<MsExceptionInfo> list);

    MsExceptionInfo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsExceptionInfo msExceptionInfo);

}
