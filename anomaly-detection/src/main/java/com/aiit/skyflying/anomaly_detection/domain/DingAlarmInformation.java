package com.aiit.skyflying.anomaly_detection.domain;

import lombok.Builder;
import lombok.Data;

import java.util.Date;

@Data
@Builder
public class DingAlarmInformation {
    private Integer id;

    private String username;

    private Date createTime;

    private Date gmtCreate;

    private Integer triggerTimes;

    private Integer ruleId;

    private Integer isDelete;
}
