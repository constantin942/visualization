package com.mingshi.skyflying.anomaly_detection.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
public class CoarseSegmentDetailOnTimeDo {
    private Integer id;

    private String username;

    private Integer counts;

    private Integer timeIntervalCount01;

    private Integer timeIntervalCount12;

    private Integer timeIntervalCount23;

    private Integer timeIntervalCount34;

    private Integer timeIntervalCount45;

    private Integer timeIntervalCount56;

    private Integer timeIntervalCount67;

    private Integer timeIntervalCount78;

    private Integer timeIntervalCount89;

    private Integer timeIntervalCount910;

    private Integer timeIntervalCount1011;

    private Integer timeIntervalCount1112;

    private Integer timeIntervalCount1213;

    private Integer timeIntervalCount1314;

    private Integer timeIntervalCount1415;

    private Integer timeIntervalCount1516;

    private Integer timeIntervalCount1617;

    private Integer timeIntervalCount1718;

    private Integer timeIntervalCount1819;

    private Integer timeIntervalCount1920;

    private Integer timeIntervalCount2021;

    private Integer timeIntervalCount2122;

    private Integer timeIntervalCount2223;

    private Integer timeIntervalCount2324;

    private Date createTime;

    private Date updateTime;

    private Boolean isDeleted;

}