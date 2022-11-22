package com.mingshi.skyflying.common.domain;

import lombok.Data;

@Data
public class UserCoarseInfo {

    private Integer id;

    private String gmtCreate;

    private String gmtModified;

    private String userName;

    private String lastVisitedDate;

    private Long visitedCount;

    private String usualVisitedData;
}
