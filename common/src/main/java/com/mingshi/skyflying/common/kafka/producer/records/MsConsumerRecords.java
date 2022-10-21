package com.mingshi.skyflying.common.kafka.producer.records;

import lombok.Data;

/**
 * <B>类名称：ConsumerRecords</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022/10/17 10:59
 **/
@Data
public class MsConsumerRecords<T> {
    /**
     * 消息类型
     */
    private Integer recordType;
    /**
     * 消息体
     */
    private T body;

    public MsConsumerRecords(Integer recordType, T body){
        this.recordType = recordType;
        this.body = body;
    }
    public MsConsumerRecords(){
    }

}
