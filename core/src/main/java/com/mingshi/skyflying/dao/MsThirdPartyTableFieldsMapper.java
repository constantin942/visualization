package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsThirdPartyTableFieldsDo;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public interface MsThirdPartyTableFieldsMapper {
  int insertSelective(MsThirdPartyTableFieldsDo record);

  MsThirdPartyTableFieldsDo selectByPrimaryKey(Integer id);

  // TODO: 2022/6/20 这里是不对的，正常来说，应该是先连接上某个数据库，然后拼接SQL，并把拼接后的SQL语句发送到上面连接上的数据库上，获取相应的表字段信息；
  List<Map<String, Object>> selectAllFields(String tableName);
  // List<Object> selectAllFields(String tableName);

  int updateByPrimaryKeySelective(MsThirdPartyTableFieldsDo record);

  void insertBatch(LinkedList<MsThirdPartyTableFieldsDo> msThirdPartyTableFieldsDos);

  List<MsThirdPartyTableFieldsDo> selectByThirdPartyNameId(List<Map<String,Object>> map);
}
