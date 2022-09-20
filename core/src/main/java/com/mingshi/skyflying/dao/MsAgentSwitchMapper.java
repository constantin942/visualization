package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsAgentSwitchDo;

import java.util.HashMap;
import java.util.List;

public interface MsAgentSwitchMapper {
    int insertSelective(MsAgentSwitchDo msAgentSwitchDo);

    MsAgentSwitchDo selectByPrimaryKey(Integer id);

    List<MsAgentSwitchDo> selectByServiceInstance(HashMap<String, Object> jsonObject);

    MsAgentSwitchDo selectByServiceInstanceLatest(String serviceInstance);

    Integer selectByServiceInstanceCount(HashMap<String, Object> jsonObject);

    int updateByPrimaryKeySelective(MsAgentSwitchDo msAgentSwitchDo);

    int updateByRequestId(MsAgentSwitchDo msAgentSwitchDo);
}
