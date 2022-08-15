package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;
@Data
public class UserPortraitByVisitedTimeDo {
    private Integer id;

    private Integer isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private Integer ruleId;

    private Integer forenoonCount;

    private Integer nightCount;

    private Integer afternoonCount;
}
