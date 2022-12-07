package com.aiit.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsSystemOperationRecord {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String systemName;

    private String agentName;

    private String serviceCode;
}
