package com.aiit.skyflying.common.domain;

import com.aiit.skyflying.common.utils.CustomJsonDateDeserializer;
import lombok.Data;
import org.codehaus.jackson.map.annotate.JsonDeserialize;

import java.util.Date;
@Data
public class MsAlarmInformationDo {
    private Integer id;

    private Integer matchRuleId;

    private Integer isDelete;

    private Integer updateUserPortrait;

    @JsonDeserialize(using= CustomJsonDateDeserializer.class)
    private Date gmtCreate;

    @JsonDeserialize(using= CustomJsonDateDeserializer.class)
    private Date gmtModified;

    @JsonDeserialize(using= CustomJsonDateDeserializer.class)
    private Date originalTime;

    private String userName;

    private String alarmContent;

    private String globalTraceId;

    /**
     * 链路请求开始的时间
     */
    private String startTime;

    /**
     * 数据库实例地址
     */
    private String dbInstance;

    /**
     * SQL语句中表的名字
     */
    private String msTableName;

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
}
