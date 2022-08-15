package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class SegmentRelationDo {
  private Integer id;

  private Byte isDelete;

  private Date gmtCreate;

  private Date gmtModified;

  private String globalTraceId;

  private String segmentIds;

  private String userName;

  private String token;
}
