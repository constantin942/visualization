package com.aiit.skyflying.domain;

import lombok.Data;

import java.util.Date;
@Data
public class SendStateRecord {
    private Long id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String state;

    private String username;

    private String traceid;

    private String service;

    private String serviceinstance;

    private String tracesegmentid;

    private String firstorsecondsend;

    private String exceptionstr;

    private String token;

    private String segmentstarttime;
}
