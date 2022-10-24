package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class SegmentDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String token;

    private String operationName;

    private String requestStartTime;

    private String globalTraceId;

    private String currentSegmentId;

    private String parentSegmentId;

    private String spans;

    private String reorganizingSpans;

    private String ip;

    /**
     * 基于访问时间维度的用户画像；
     */
    private Integer userPortraitFlagByVisitedTime;

    /**
     * 基于访问过的表维度的用户画像；
     */
    private Integer userPortraitFlagByVisitedTable;

    private String serviceCode;

    private String serviceInstanceName;

    /**
     * 假设商城应用 Mall 调用了 Order 这个订单应用，那么对于 Order 应用来说，parentService就是 Mall；2022-04-22 16:12:46
     */
    private String parentService;
    /**
     * 一个应用可能部署了多个实例，这个parentServiceInstance就记录了 parentService 的一个具体实例；2022-04-22 16:14:18
     */
    private String parentServiceInstance;
    /**
     * 进入 parentService 的那个请求；
     */
    private String parentEndpoint;
}
