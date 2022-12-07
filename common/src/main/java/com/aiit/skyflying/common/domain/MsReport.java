package com.aiit.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsReport {
    private Integer id;

    private Integer isDelete;

    private Date gmtCreate;

    private Date gmtModified;
}
