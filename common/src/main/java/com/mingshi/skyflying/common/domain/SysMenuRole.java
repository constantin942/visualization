package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class SysMenuRole {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String creator;

    private String modifier;

    private Integer menuId;

    private Integer roleId;
}
