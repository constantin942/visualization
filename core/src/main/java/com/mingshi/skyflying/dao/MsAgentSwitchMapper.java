package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.common.domain.MsAgentSwitchDo;

import java.util.HashMap;
import java.util.List;

public interface MsAgentSwitchMapper {
    int insertSelective(MsAgentSwitchDo record);

    MsAgentSwitchDo selectByPrimaryKey(Integer id);

    List<MsAgentSwitchDo> selectByServiceInstance(HashMap jsonObject);

    MsAgentSwitchDo selectByServiceInstanceLatest(String serviceInstance);

    Integer selectByServiceInstanceCount(HashMap jsonObject);

    int updateByPrimaryKeySelective(MsAgentSwitchDo msAgentSwitchDo);

    int updateByRequestId(MsAgentSwitchDo msAgentSwitchDo);
}
