package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class UserPortraitRulesDo {
    private Integer id;

    private Integer isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String ruleName;

    private String ruleDesc;

    private String ruleDescDetail;
}
