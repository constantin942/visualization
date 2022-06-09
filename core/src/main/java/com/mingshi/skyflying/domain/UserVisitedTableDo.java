package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;
@Data
public class UserVisitedTableDo {
    private Integer id;

    private String userName;

    private String requestStartTime;

    private Date gmtCreate;

    private Date gmtModified;

    private Byte isDelete;
}
