package com.mingshi.skyflying.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsThirdPartyTableFieldsDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private Integer thirdPartyNameId;

    private String field;

    private String feldType;

    private String fieldName;

    private String fieldNote;
}
