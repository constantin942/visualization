package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.MsAgentInformationDo;
import com.mingshi.skyflying.common.response.ServerResponse;


public interface MsAgentInformationService extends ParentService<MsAgentInformationDo, Long> {
  ServerResponse<String> getAllSkywalkingAgent(String agentCode, Integer pageNo, Integer pageSize);

  ServerResponse<String> updateSkywalkingAgent(Integer id, String agentName);

  ServerResponse<String> getActiveSkywalkingAgent();
}
