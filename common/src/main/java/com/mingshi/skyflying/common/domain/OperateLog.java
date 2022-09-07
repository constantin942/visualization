package com.mingshi.skyflying.common.domain;

import lombok.Data;

@Data
public class OperateLog {
    private Long id;

    private String gmtCreate;

    private String gmtModified;

    private String userName;

    private String loginIp;

    private String methodName;

    private String requestUrl;

    private String requestParams;

    private String responseParams;

    private String orderId;
}
