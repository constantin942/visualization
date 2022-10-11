package com.mingshi.skyflying.anomaly_detection.service;

import com.mingshi.skyflying.common.domain.UserPortraitRulesDo;
import com.mingshi.skyflying.common.response.ServerResponse;


public interface UserPortraitRulesService extends ParentService<UserPortraitRulesDo, Long> {
  ServerResponse<String> getAllUserPortraitRules(Integer pageNo, Integer pageSize);

  ServerResponse<String> updateUserPortraitRule(Integer ruleId, Integer isDelete);

    boolean cacheRule(Integer ruleId, Integer isDelete);

    void updateRule(Integer ruleId, Integer isDelete);
}
