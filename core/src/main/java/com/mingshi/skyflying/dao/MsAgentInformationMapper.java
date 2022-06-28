package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsAgentInformationDo;

import java.util.LinkedList;
import java.util.List;

public interface MsAgentInformationMapper {

  int insertSelective(MsAgentInformationDo record);

  MsAgentInformationDo selectByPrimaryKey(Integer id);

  List<MsAgentInformationDo> selectAll();

  int updateByPrimaryKeySelective(MsAgentInformationDo record);

  void insertBatch(LinkedList<MsAgentInformationDo> list);
}
