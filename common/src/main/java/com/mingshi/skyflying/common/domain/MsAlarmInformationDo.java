package com.mingshi.skyflying.common.domain;

import com.mingshi.skyflying.common.utils.CustomJsonDateDeserializer;
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
}
