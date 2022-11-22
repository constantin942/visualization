package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class RealTimeStatisticsUserPortraitByTable {
    private Integer id;

    private String userName;

    private String usualVisitedData;

    private Integer visitedCount;

    private Date createTime;

    private Date updateTime;

    private Integer isDeleted;

    private String lastVisitedDate;
}
