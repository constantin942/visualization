package com.mingshi.skyflying.common.enums;

/**
 * <B>类名称：RecordEnum</B>
 * <B>概要说明：消息类型枚举类</B>
 *
 * @Author zm
 * @Date 2022/10/17 11:01
 **/
public enum RecordEnum {
    MSSEGMENTDETAILDO_CONSUME_FAILED(0, "消费失败"),
    ANOMALY_ALARM(1, "异常告警"),
    SEND_RECORDS_STATE(2, "探针已发送消息记录信息"),
    AGENT_SWITCH(3, "探针开关信息"),
    EXCEPTION_INFO(4, "异常信息")
    ;
    private Integer code;

    private String desc;

    RecordEnum(Integer code, String desc) {
        this.code = code;
        this.desc = desc;
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getDesc() {
        return desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }
}
