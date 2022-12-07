package com.aiit.skyflying.anomaly_detection.domain;

import lombok.Builder;
import lombok.Data;

import java.util.Date;
@Builder
@Data
public class UserPortraitByTimeDo {
    private Integer id;

    private String username;

    private Double morningRate;

    private Double afternoonRate;

    private Double nightRate;

    private Date createTime;

    private Date updateTime;
}
