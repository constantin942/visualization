package com.mingshi.skyflying.anomaly_detection.domain;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class PortraitConfig {

    private Integer id;

    /**
     * 库表画像统计周期
     */
    @NotNull(message = "库表画像统计周期必传")
    private Integer ruleTablePeriod;

    /**
     * 库表画像告警阈值
     */
    @NotNull(message = "库表画像告警阈值必传")
    private Integer ruleTableCount;

    /**
     * 时间画像统计周期
     */
    @NotNull(message = "时间画像统计周期必传")
    private Integer ruleTimePeriod;

    /**
     * 时间画像告警阈值
     */
    @NotNull(message = "时间画像告警阈值必传")
    private Double ruleTimeRate;

    private Boolean isDeleted;
}