package com.mingshi.skyflying.domain;

import lombok.Data;
import java.util.Date;

@Data
public class UserPortraitByVisitedTableDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String ruleName;

    private String visitedTable;

    private Integer visitedCount;
}
