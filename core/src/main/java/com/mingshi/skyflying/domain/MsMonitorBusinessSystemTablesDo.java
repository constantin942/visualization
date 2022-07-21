package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsMonitorBusinessSystemTablesDo {
    private Integer id;

    private Integer isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String systemName;

    private String tableName;

    private String dbName;

    private String dbAddress;

    private String tableDesc;
}
