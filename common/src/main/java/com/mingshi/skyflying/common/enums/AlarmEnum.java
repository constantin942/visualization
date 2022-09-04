package com.mingshi.skyflying.common.enums;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/9/4
 */
public enum AlarmEnum {
    NEW_USER(0, "新用户"),
    TIME_ALARM(1, "时间"),
    TABLE_ALARM(2, "库表");

    private Integer code;

    private String desc;

    AlarmEnum(Integer code, String desc) {
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
