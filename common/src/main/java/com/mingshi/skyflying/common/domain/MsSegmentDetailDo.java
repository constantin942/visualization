package com.mingshi.skyflying.common.domain;

import lombok.Data;

import java.util.Date;

@Data
public class MsSegmentDetailDo implements Cloneable {

    private Date gmtCreate;

    private Date gmtModified;

    private int isDelete;

    /**
     * 记录在数据库中的自增id
     */
    private Integer id;

    /**
     * 登录系统的用户所在的机器ip
     */
    private String userLoginIp;

    /**
     * 登录系统的用户名
     */
    private String userName;

    /**
     * 与用户名绑定
     */
    private String token;

    /**
     * 这条链路信息中唯一的标识
     */
    private String globalTraceId;

    /**
     * 当前segment实例信息的父segment实例id
     */
    private String parentSegmentId;

    /**
     * url地址
     */
    private String operationName;

    /**
     * 一条完整的链路信息由若干个segment实例信息组成；
     * 每个segment实例都有自己的segmentId；
     * 这些segment实例信息也是有顺序的，这个顺序是一个请求执行代码的先后访问顺序；
     * 在一个微服务内部，一个segment有若干个span实例信息组成；
     */
    private String currentSegmentId;

    /**
     * 一条链路信息由1到多个span信息组成，
     * 一个span就是一次访问请求，比如：访问了一次数据库操作，或者访问了一次redis缓存操作等。
     * 这些span是有顺序的。
     */
    private Integer spanId;

    /**
     * 当前链路信息是由哪一个插件采集到的
     */
    private String component;

    /**
     * 运维人员将探针与应用系统一起启动时，给应用系统起的名称；
     */
    private String serviceCode;

    /**
     * 如果这条链路信息中包含了SQL语句，那这个字段的值就是数据库实例的地址
     */
    private String peer;

    /**
     * 当前链路信息是由哪一个插件中的哪一个方法采集到的
     */
    private String endpointName;

    /**
     * 链路请求开始的时间
     */
    private String startTime;

    /**
     * 探针的唯一标识
     */
    private String serviceInstanceName;

    /**
     * 链路请求结束的时间
     */
    private String endTime;

    private Integer parentSpanId;

    /**
     * 如果这条链路信息有SQL语句，那这个字段的值就是sql，如果没有SQL语句，那这个字段的值就是url
     */
    private String operationType;

    /**
     * SQL语句中表的名字
     */
    private String msTableName;

    /**
     * SQL语句的类型，比如：select、insert等
     */
    private String dbType;

    /**
     * 数据库实例地址
     */
    private String dbInstance;

    /**
     * 数据库用户名
     */
    private String dbUserName;

    /**
     * 具体的SQL语句
     */
    private String dbStatement;

    /**
     * 假设商城应用 Mall 调用了 Order 这个订单应用，那么对于 Order 应用来说，parentService就是 Mall；2022-04-22 16:12:46
     */
    private String parentService;
    /**
     * 一个应用可能部署了多个实例，这个parentServiceInstance就记录了 parentService 的一个具体实例；2022-04-22 16:14:18
     */
    private String parentServiceInstance;
    /**
     * 进入 parentService 的那个请求；
     */
    private String parentEndpoint;
    /**
     * 当前用户来自哪里；
     */
    private String userFrom;

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

}
