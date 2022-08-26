package com.mingshi.skyflying.anomaly_detection.domain;

import java.util.Date;

public class UserPortraitByTimeDo {
    private Integer id;

    private String username;

    private Double morningRate;

    private Double afternoonRate;

    private Double nightRate;

    private Date createTime;

    private Date updateTime;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public Double getMorningRate() {
        return morningRate;
    }

    public void setMorningRate(Double morningRate) {
        this.morningRate = morningRate;
    }

    public Double getAfternoonRate() {
        return afternoonRate;
    }

    public void setAfternoonRate(Double afternoonRate) {
        this.afternoonRate = afternoonRate;
    }

    public Double getNightRate() {
        return nightRate;
    }

    public void setNightRate(Double nightRate) {
        this.nightRate = nightRate;
    }

    public Date getCreateTime() {
        return createTime;
    }

    public void setCreateTime(Date createTime) {
        this.createTime = createTime;
    }

    public Date getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }
}