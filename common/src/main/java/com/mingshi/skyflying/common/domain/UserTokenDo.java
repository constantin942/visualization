package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class UserTokenDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String token;

    private String globalTraceId;
}
