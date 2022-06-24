package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.UserPortraitRulesDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface UserPortraitRulesService extends ParentService<UserPortraitRulesDo, Long> {
  ServerResponse<String> getAllUserPortraitRules(Integer pageNo, Integer pageSize);

  ServerResponse<String> updateUserPortraitRule(Integer ruleId, Integer isDelete);
}
