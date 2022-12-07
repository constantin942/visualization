package com.aiit.skyflying.common.domain;

import lombok.Data;
import java.util.Date;

@Data
public class SysMenu {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String creator;

    private Byte type;

    private String title;

    private String href;
}
