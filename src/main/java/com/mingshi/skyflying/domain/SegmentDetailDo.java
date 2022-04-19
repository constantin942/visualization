package com.mingshi.skyflying.domain;

import lombok.Data;

@Data
public class SegmentDetailDo {
    private String id;

    private String segmentId;

    private String traceId;

    private String serviceId;

    private String serviceInstanceId;

    private String endpointId;

    private Long startTime;

    private Integer latency;

    private Integer isError;

    private String tags;

    private String token;

    private String userName;

    private Long timeBucket;

    private String dataBinary;
}
