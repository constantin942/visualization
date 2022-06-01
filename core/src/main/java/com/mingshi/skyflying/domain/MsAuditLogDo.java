package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsAuditLogDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String opTime;

    private String operationName;

    private String applicationUserName;

    private String parentSegmentId;

    private String currentSegmentId;

    private String globalTraceId;

    private String userName;

    private Long userId;

    private String instanceName;

    private Long instanceId;

    private String msSchemaName;

    private Long dbId;

    private Boolean logic;

    private String SqlType;

    private String execState;

    private Long affectRows;

    private Long elapsedTime;

    private String remark;

    private String msSql;

    private String sqlSource;

    private String hash;

    private String sqlInsightDbUserName;

    private String sqlInsightUserIp;

    private String msTableName;

}
