package com.mingshi.skyflying.disruptor.processor;

import lombok.Data;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

/**
 * @ClassName FlowsDataWapper
 * @Description TODO
 * @Author apple
 * Date 2021/11/14 12:32
 * @Version 1.0
 **/
@Data
public class SegmentByByte {

  ConsumerRecord<String, Bytes> record;

  public SegmentByByte() {
  }

  public void setSegmentByByte(ConsumerRecord<String, Bytes> record) {
    this.record = record;
  }
}
