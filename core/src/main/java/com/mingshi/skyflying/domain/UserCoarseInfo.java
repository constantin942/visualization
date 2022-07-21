package com.mingshi.skyflying.domain;

import lombok.Data;

@Data
public class UserCoarseInfo {

    private String userName;

    private String lastVisitedDate;

    private Long visitedCount;

    private String usualVisitedData;
}
