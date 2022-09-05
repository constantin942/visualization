package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;
@Data
public class MsSegmentDetailDo {
    private Integer id;

    private String ip;

    private String userName;

    private String token;

    private String globalTraceId;

    private String parentSegmentId;

    private String operationName;

    private String currentSegmentId;

    private Date gmtCreate;

    private Date gmtModified;

    private int isDelete;

    private Integer spanId;

    private String component;

    private String serviceCode;

    private String peer;

    private String endpointName;

    private String startTime;

    private String serviceInstanceName;

    private String endTime;

    private Integer parentSpanId;

    private String operationType;

    private String msTableName;

    private String dbType;

    private String dbInstance;

    private String dbUserName;

    private String dbStatement;

    private Integer userPortraitFlagByVisitedTime;

    private Integer userPortraitFlagByVisitedTableEveryday;
}
