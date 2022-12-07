package com.aiit.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class SysOperatorRole {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String creator;

    private String modifier;

    private Integer operatorId;

    private Integer roleId;
}
