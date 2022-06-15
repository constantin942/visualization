package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;
@Data
public class MsDmsAuditLogDo {
    private Integer id;

    private String sqlInsightDbUserName;

    private String sqlSource;

    private String msTableName;

    private String opTime;

    private String userName;

    private String instanceName;

    private Long instanceId;

    private String msSchemaName;

    private Long dbId;

    private Boolean logic;

    private String sqlType;

    private String execState;

    private Long affectRows;

    private Long elapsedTime;

    private String remark;

    private Long userId;

    private Date gmtCreate;

    private Date gmtModified;

    private Byte isDelete;

    private String hash;

    private String sqlInsightUserIp;

    private String msSql;
}
