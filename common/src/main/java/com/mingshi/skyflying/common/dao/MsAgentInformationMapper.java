package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.MsAgentInformationDo;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public interface MsAgentInformationMapper {

  MsAgentInformationDo selectByPrimaryKey(Integer id);

  List<MsAgentInformationDo> selectAll();

  int updateByPrimaryKeySelective(MsAgentInformationDo msAgentInformationDo);

  void insertBatch(LinkedList<MsAgentInformationDo> list);

  List<MsAgentInformationDo> selectAllAgents(Map<String, Object> queryMap);

  Integer selectAllAgentsCount(Map<String, Object> queryMap);

  String selectByAgentCode(String serviceCode);
}
