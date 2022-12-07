package com.aiit.skyflying.common.domain;

import lombok.Data;

@Data
public class UserLoginStatistics {
    private Integer id;

    private Integer isDelete;

    private String gmtCreate;

    private String gmtModified;

    private String userName;

    private Integer passwordErrorCount;

    private String description;
}
