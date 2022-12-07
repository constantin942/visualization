package com.aiit.skyflying.common.domain;

import lombok.Data;

@Data
public class MsAgentSwitchDo {
    private Integer id;

    private Byte isDelete;

    private String gmtCreate;

    private String gmtModified;

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
