package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsThirdPartyTableListDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String thirdPartyName;

    private String thirdPartyDbName;

    private String thirdPartyTableName;

    private String tableDesc;
}
