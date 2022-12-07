package com.aiit.skyflying.anomaly_detection.domain;

import lombok.Data;

/**
 * @Author: 唐郑翔
 * @Description: 粗粒度表生成的用户在画像周期内早中晚访问次数
 * @Date: create in 2022/8/29
 */
@Data
public class VisitCountOnTimeInterval {
    /**
     * 用户名
     */
    private String username;
    /**
     * 周期内总访问次数
     */
    private Integer counts;
    /**
     * 上午访问次数(5-13)
     */
    private Integer morningCount;
    /**
     * 下午访问次数(13-21)
     */
    private Integer afternoonCount;
    /**
     * 夜间访问次数(21-5)
     */
    private Integer nightCount;

}
