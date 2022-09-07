package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class UserLoginLog {
    private Integer id;

    private Date gmtCreate;

    private String userName;

    private String loginIp;

    private String result;

    private Date gmtModified;

    private String sessionId;

    private String description;

}
