package com.mingshi.skyflying.elasticsearch.domain;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Document(indexName = "segment_detail", shards = 3, replicas = 1)
public class EsMsSegmentDetailDo {

  // @Field(type=FieldType.Text, analyzer=“ik_max_word”) 表示该字段是一个文本，并作最大程度拆分，默认建立索引
  // @Field(type=FieldType.Text,index=false) 表示该字段是一个文本，不建立索引
  // @Field(type=FieldType.Date) 表示该字段是一个文本，日期类型，默认不建立索引
  // @Field(type=FieldType.Long) 表示该字段是一个长整型，默认建立索引
  // @Field(type=FieldType.Keyword) 表示该字段内容是一个文本并作为一个整体不可分，默认建立索引
  // @Field(type=FieldType.Float) 表示该字段内容是一个浮点类型并作为一个整体不可分，默认建立索引

  // date 、float、long都是不能够被拆分的

  @Id
  private Integer id;

  /**
   * type : 字段数据类型
   * analyzer : 分词器类型
   * index : 是否索引(默认:true)
   * Keyword : 短语,不进行分词
   */
  // @Field(type = FieldType.Text, analyzer = "ik_max_word")
  @Field(type = FieldType.Keyword, index = true)
  private String userName;

  @Field(type = FieldType.Keyword, index = true)
  private String token;

  @Field(type = FieldType.Keyword, index = true)
  private String globalTraceId;

  @JsonFormat(pattern="YYYY-MM-dd'T'HH:mm:ss",timezone="GMT+8")
  @Field(type = FieldType.Date, index = true)
  private Date startTime;

  @JsonFormat(pattern="YYYY-MM-dd'T'HH:mm:ss",timezone="GMT+8")
  @Field(type = FieldType.Date, index = true)
  private Date endTime;

  @Field(type = FieldType.Keyword, index = true)
  private String operationType;

  @Field(type = FieldType.Keyword, index = true)
  private String msTableName;

  @Field(type = FieldType.Keyword, index = true)
  private String dbType;

  @Field(type = FieldType.Keyword, index = true)
  private String dbInstance;

  @Field(type = FieldType.Keyword, index = true)
  private String dbUserName;

  @JsonFormat(pattern="YYYY-MM-dd'T'HH:mm:ss",timezone="GMT+8")
  @Field(type = FieldType.Date, index = false)
  private Date gmtCreate;

  @JsonFormat(pattern="YYYY-MM-dd'T'HH:mm:ss",timezone="GMT+8")
  @Field(type = FieldType.Date, index = false)
  private Date gmtModified;

  @Field(type = FieldType.Keyword, index = false)
  private String parentSegmentId;

  @Field(type = FieldType.Keyword, index = false)
  private String operationName;

  @Field(type = FieldType.Keyword, index = false)
  private String currentSegmentId;

  @Field(type = FieldType.Keyword, index = false)
  private int isDelete;

  @Field(type = FieldType.Keyword, index = false)
  private Integer spanId;

  @Field(type = FieldType.Keyword, index = false)
  private String component;

  @Field(type = FieldType.Keyword, index = false)
  private String serviceCode;

  @Field(type = FieldType.Keyword, index = false)
  private String peer;

  @Field(type = FieldType.Keyword, index = false)
  private String endpointName;

  @Field(type = FieldType.Keyword, index = false)
  private String serviceInstanceName;

  @Field(type = FieldType.Keyword, index = false)
  private Integer parentSpanId;

  @Field(type = FieldType.Keyword, index = false)
  private String dbStatement;

  @Field(type = FieldType.Integer, index = false)
  private Integer userPortraitFlagByVisitedTime;

  @Field(type = FieldType.Integer, index = false)
  private Integer userPortraitFlagByVisitedTableEveryday;
}
