package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.MsUserFrom;

import java.util.List;
import java.util.Map;

public interface MsUserFromMapper {
    int insertSelective(MsUserFrom msUserFrom);

    MsUserFrom selectByPrimaryKey(Integer id);

    MsUserFrom selectByUserFromPath(String userFromPath);

    List<MsUserFrom> selectAll(Map<String, Integer> queryMap);

    Integer selectAllCount();

    int updateByPrimaryKeySelective(MsUserFrom msUserFrom);
}
