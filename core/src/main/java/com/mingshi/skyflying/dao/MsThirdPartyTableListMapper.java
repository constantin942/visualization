package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsThirdPartyTableListDo;

import java.util.List;
import java.util.Map;

public interface MsThirdPartyTableListMapper {

    int insertSelective(MsThirdPartyTableListDo record);

    MsThirdPartyTableListDo selectByPrimaryKey(Integer id);

  // TODO: 2022/6/20 正常来说，应该是连接到指定的数据库，然后拼接SQL，把拼接后的SQL发送到数据库中；
    List<String> selectAllTables(String dbName);

    List<MsThirdPartyTableListDo> selectAllTablesByDbName(String dbName);

    int updateByPrimaryKeySelective(MsThirdPartyTableListDo record);

    void insertBatch(List<MsThirdPartyTableListDo> msThirdPartyTableListDoList);

    List<Map<String,Object>> selectByDbNameAndTableName(Map<String, Object> queryMap);
}
