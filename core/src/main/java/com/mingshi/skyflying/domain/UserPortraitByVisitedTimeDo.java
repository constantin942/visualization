package com.mingshi.skyflying.domain;

import java.util.Date;

public class UserPortraitByVisitedTimeDo {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String ruleName;

    private Integer forenoonCount;

    private Integer nightCount;

    private Integer afternoonCount;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Byte getIsDelete() {
        return isDelete;
    }

    public void setIsDelete(Byte isDelete) {
        this.isDelete = isDelete;
    }

    public Date getGmtCreate() {
        return gmtCreate;
    }

    public void setGmtCreate(Date gmtCreate) {
        this.gmtCreate = gmtCreate;
    }

    public Date getGmtModified() {
        return gmtModified;
    }

    public void setGmtModified(Date gmtModified) {
        this.gmtModified = gmtModified;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getRuleName() {
        return ruleName;
    }

    public void setRuleName(String ruleName) {
        this.ruleName = ruleName;
    }

    public Integer getForenoonCount() {
        return forenoonCount;
    }

    public void setForenoonCount(Integer forenoonCount) {
        this.forenoonCount = forenoonCount;
    }

    public Integer getNightCount() {
        return nightCount;
    }

    public void setNightCount(Integer nightCount) {
        this.nightCount = nightCount;
    }

    public Integer getAfternoonCount() {
        return afternoonCount;
    }

    public void setAfternoonCount(Integer afternoonCount) {
        this.afternoonCount = afternoonCount;
    }
}
