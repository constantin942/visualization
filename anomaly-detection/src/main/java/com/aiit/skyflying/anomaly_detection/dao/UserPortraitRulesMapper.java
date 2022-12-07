package com.aiit.skyflying.anomaly_detection.dao;

import com.aiit.skyflying.common.domain.UserPortraitRulesDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitRulesMapper {

    int insertSelective(UserPortraitRulesDo msg);

    UserPortraitRulesDo selectByPrimaryKey(Integer id);

    UserPortraitRulesDo selectByRuleName(String ruleName);

    int updateByPrimaryKeySelective(UserPortraitRulesDo msg);

    List<UserPortraitRulesDo> selectAllRules(Map<String, Object> queryMap);

    List<UserPortraitRulesDo> selectAllEnableRules();

    Integer selectAllRulesCount(Map<String, Object> queryMap);
}
