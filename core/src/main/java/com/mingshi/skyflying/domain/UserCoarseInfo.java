package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;

@Data
public class UserCoarseInfo {

    private String userName;

    private Date lastVisitedDate;

    private Long visitedCount;

    private Date usualVisitedData;
}
