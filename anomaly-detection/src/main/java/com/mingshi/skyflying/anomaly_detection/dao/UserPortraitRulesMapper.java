package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.common.domain.UserPortraitRulesDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitRulesMapper {

    int insertSelective(UserPortraitRulesDo msg);

    UserPortraitRulesDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitRulesDo msg);

    List<UserPortraitRulesDo> selectAllRules(Map<String, Object> queryMap);

    List<UserPortraitRulesDo> selectAllEnableRules();

    Integer selectAllRulesCount(Map<String, Object> queryMap);
}
