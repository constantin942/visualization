package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsThirdPartyTableListDo;

import java.util.List;
import java.util.Map;

public interface MsThirdPartyTableListMapper {

    int insertSelective(MsThirdPartyTableListDo msThirdPartyTableListDo);

    MsThirdPartyTableListDo selectByPrimaryKey(Integer id);

    List<String> selectAllTables(String dbName);

    MsThirdPartyTableListDo selectByTableName(String tableName);

    List<MsThirdPartyTableListDo> selectAllTablesByDbName(String dbName);

    int updateByPrimaryKeySelective(MsThirdPartyTableListDo msThirdPartyTableListDo);

    void insertBatch(List<MsThirdPartyTableListDo> msThirdPartyTableListDoList);

    List<Map<String,Object>> selectByDbNameAndTableName(Map<String, Object> queryMap);
}
