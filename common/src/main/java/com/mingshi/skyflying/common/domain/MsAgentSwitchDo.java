package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsAgentSwitchDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String serviceInstance;

    private String agentSwitchStatus;

    private String operator;

    private String sendKafkaRequestParams;

    private String sendKafkaStatus;

    private String receiveKafkaResponseParams;

    private String receiveKafkaStatus;

    private String requestId;

    private String operationType;
}
