package com.mingshi.skyflying.common.domain;
import lombok.Data;

@Data
public class TableCoarseInfo {
    private String tableName;

    private String tableNameDesc;

    private String lastVisitedDate;

    private Long visitedCount;

    private String usualVisitedUser;
}