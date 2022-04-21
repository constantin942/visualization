package com.mingshi.skyflying.domain;

import lombok.Data;
import java.util.Date;

@Data
public class Segment {
    private Integer id;

    private Byte isDelete;

    private Date gmtCreate;

    private Date gmtModified;

    private String userName;

    private String token;

    private String operationName;

    private String requestStartTime;

    private String spans;

    private String reorganizingSpans;
}
