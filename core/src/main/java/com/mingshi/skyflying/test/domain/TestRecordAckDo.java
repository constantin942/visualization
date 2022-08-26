package com.mingshi.skyflying.test.domain;

import lombok.Data;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.kafka.support.Acknowledgment;

/**
 * <B>主类名称: TestRecordAckDo</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/20 16:34
 *
 * @Version 1.0
 **/
@Data
public class TestRecordAckDo {
  private ConsumerRecord<String, Bytes> record;
  private Acknowledgment ack;
}
