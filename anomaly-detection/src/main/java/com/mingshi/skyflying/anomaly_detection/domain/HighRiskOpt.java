package com.mingshi.skyflying.anomaly_detection.domain;


import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class HighRiskOpt {
    private Integer id;

    private String keyword;

    private String description;

    private Integer enable;

    private String alarmInfo;

    private Integer isDeleted;
}