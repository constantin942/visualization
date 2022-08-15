package com.mingshi.skyflying.common.domain;

import lombok.Data;
import java.util.Date;

@Data
public class MsScheduledTaskDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String startTime;

    private String endTime;

    private Integer pageSize;

    private Integer pageNumber;

    private String status;

    private Integer recordCount;
}
