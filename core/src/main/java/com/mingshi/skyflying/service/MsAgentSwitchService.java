package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.MsAgentSwitchDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.service.ParentService;


public interface MsAgentSwitchService extends ParentService<MsAgentSwitchDo, Long> {
  ServerResponse<String> allAgentOperationRecord(String serviceInstance, Integer pageNo, Integer pageSize);

  ServerResponse<String> updateAgentStatus(String serviceInstance, String agentSwitch);

  ServerResponse<String> queryAgentStatus(String serviceInstance);
}
