package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;
@Data
public class MsAlarmInformationDo {
    private Integer id;

    private Integer matchRuleId;

    private Integer isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private Date originalTime;

    private String userName;

    private String alarmContent;

    private String globalTraceId;
}
