package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsExceptionInfo;

import java.util.List;

public interface MsExceptionInfoMapper {

    int insertSelective(MsExceptionInfo msExceptionInfo);

    int insertSelectiveBatch(List<MsExceptionInfo> list);

    MsExceptionInfo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsExceptionInfo msExceptionInfo);

}
