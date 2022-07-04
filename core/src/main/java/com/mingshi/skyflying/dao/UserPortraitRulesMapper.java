package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.UserPortraitRulesDo;

import java.util.List;
import java.util.Map;

public interface UserPortraitRulesMapper {

    int insertSelective(UserPortraitRulesDo record);

    UserPortraitRulesDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(UserPortraitRulesDo record);

    List<UserPortraitRulesDo> selectAllRules(Map<String, Object> queryMap);

    List<UserPortraitRulesDo> selectAllEnableRules();

    Integer selectAllRulesCount(Map<String, Object> queryMap);
}