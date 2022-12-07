package com.aiit.skyflying.common.domain;

import lombok.Data;

import java.util.Date;
@Data
public class UserPortraitByVisitedTableEverydayDo {
    private Integer id;

    private Integer isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String ruleName;

    private String visitedTable;

    private Integer visitedCount;

    private String visitedDate;

    private String dbType;

    private String visitedDbInstance;

}
