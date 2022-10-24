package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;
@Data
public class MsExceptionInfo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String serviceInstanceName;

    private String serviceCode;

    private String exceptionInfo;
}
