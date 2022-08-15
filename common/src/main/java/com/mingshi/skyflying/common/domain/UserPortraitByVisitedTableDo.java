package com.mingshi.skyflying.common.domain;

import lombok.Data;
import java.util.Date;

@Data
public class UserPortraitByVisitedTableDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private Integer ruleId;

    private String visitedTable;

    private Integer visitedCount;
}
