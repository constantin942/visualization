package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;
@Data
public class MsAlarmInformationDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String alarmContent;

    private String globalTraceId;
}
