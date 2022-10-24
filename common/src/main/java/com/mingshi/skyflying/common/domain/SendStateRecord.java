package com.mingshi.skyflying.common.domain;

import lombok.Data;

/**
 * <B>类名称：</B>
 * <B>概要说明：记录探针已发送的记录信息，用于调试没有用户名的情况</B>
 *
 * @Author zm
 * @Date 2022-10-22 18:06:21
 * @Param
 * @return
 **/
@Data
public class SendStateRecord {
    /**
     * 探针发送该消息时的发送的状态
     */
    String state;

    /**
     * 用户名
     */
    String userName;

    /**
     * 用户登录的token
     */
    String token;

    /**
     * 一条链路的全局id
     */
    String traceId;

    /**
     * 探针对应的服务
     */
    String service;

    /**
     * 探针标识
     */
    String serviceInstance;

    /**
     * 当前segment的id
     */
    String traceSegmentId;

    /**
     * 产生当前segment的时间
     */
    Long segmentStartTime;

    /**
     * 当前segment是第一次发送。如果第一次发送成功，这个标识就是first_send.如果第一次发送失败，当前消息被写入文件，然后再次发送出去，那么此时这个标识是second_send。
     */
    String firstOrSecondSend;

    /**
     * 是否出现了异常信息
     */
    String exceptionStr;
}
