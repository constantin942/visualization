package com.aiit.skyflying.common.domain;

import lombok.Data;
import java.util.Date;

@Data
public class MsConfigDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String config;

    private String configType;
}
