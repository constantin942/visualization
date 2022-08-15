package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class SegmentDo {
  private Integer id;

  private Byte isDelete;

  private Date gmtCreate;

  private Date gmtModified;

  private String userName;

  private String token;

  private String operationName;

  private String requestStartTime;

  private String globalTraceId;

  private String currentSegmentId;

  private String parentSegmentId;

  private String spans;

  private String reorganizingSpans;

  /**
   * 基于访问时间维度的用户画像；
   */
  private Integer userPortraitFlagByVisitedTime;

  /**
   * 基于访问过的表维度的用户画像；
   */
  private Integer userPortraitFlagByVisitedTable;

  private String serviceCode;

  private String serviceInstanceName;
}
