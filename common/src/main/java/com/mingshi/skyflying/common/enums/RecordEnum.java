package com.mingshi.skyflying.common.enums;

/**
 * <B>类名称：RecordEnum</B>
 * <B>概要说明：消息类型枚举类</B>
 *
 * @Author zm
 * @Date 2022/10/17 11:01
 **/
public enum RecordEnum {
    MsSegmentDetailDo_Consume_Failed(0, "消费失败"),
    Anomaly_ALARM(1, "异常告警");
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
