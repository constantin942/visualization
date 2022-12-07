package com.aiit.skyflying.service;

import com.aiit.skyflying.common.domain.MsAgentInformationDo;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;


public interface MsAgentInformationService extends ParentService<MsAgentInformationDo, Long> {
  ServerResponse<String> getAllSkywalkingAgent(String agentCode, Integer pageNo, Integer pageSize);

  ServerResponse<String> updateSkywalkingAgent(Integer id, String agentName);

  ServerResponse<String> getActiveSkywalkingAgent();
}
