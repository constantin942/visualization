package com.mingshi.skyflying.domain;
import lombok.Data;

import java.util.Date;

@Data
public class TableCoarseInfo {
    private String tableName;

    private Date lastVisitedDate;

    private Long visitedCount;

    private String usualVisitedUser;
}
