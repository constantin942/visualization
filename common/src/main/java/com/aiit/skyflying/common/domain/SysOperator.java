package com.aiit.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class SysOperator {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String creator;

    private String modifier;

    private Byte status;

    private String userName;

    private String password;

    private String salt;

    private String phone;

    private String name;

    private String email;
}
