package com.mingshi.skyflying.anomaly_detection.domain;

import lombok.Data;

@Data
public class DingAlarmConfig {
    private Integer id;

    private String webhook;

    private String secret;

    private Integer gap;

    private String mobiles;
}