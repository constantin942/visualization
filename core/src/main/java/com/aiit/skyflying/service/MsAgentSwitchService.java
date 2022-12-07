package com.aiit.skyflying.service;

import com.aiit.skyflying.common.domain.MsAgentSwitchDo;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;


public interface MsAgentSwitchService extends ParentService<MsAgentSwitchDo, Long> {
  ServerResponse<String> allAgentOperationRecord(String serviceInstance, Integer pageNo, Integer pageSize);

  ServerResponse<String> updateAgentStatus(String serviceInstance, String agentSwitch);

  ServerResponse<String> queryAgentStatus(String serviceInstance);
}
