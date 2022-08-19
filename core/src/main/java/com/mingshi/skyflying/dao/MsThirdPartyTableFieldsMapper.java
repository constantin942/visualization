package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsThirdPartyTableFieldsDo;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public interface MsThirdPartyTableFieldsMapper {
  int insertSelective(MsThirdPartyTableFieldsDo msThirdPartyTableFieldsDo);

  MsThirdPartyTableFieldsDo selectByPrimaryKey(Integer id);

  List<Map<String, Object>> selectAllFields(String tableName);

  int updateByPrimaryKeySelective(MsThirdPartyTableFieldsDo msThirdPartyTableFieldsDo);

  void insertBatch(LinkedList<MsThirdPartyTableFieldsDo> msThirdPartyTableFieldsDos);

  List<MsThirdPartyTableFieldsDo> selectByThirdPartyNameId(List<Map<String,Object>> map);
}
