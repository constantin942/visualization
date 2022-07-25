package com.mingshi.skyflying.disruptor.processor;

import lombok.Data;

/**
 * @ClassName FlowsDataWapper
 * @Description TODO
 * Author apple
 * Date 2021/11/14 12:32
 * @Version 1.0
 **/
@Data
public class SegmentByByte {

  private byte[] data;

  public SegmentByByte() {
  }

  public void setSegmentByByte(byte[] data) {
    this.data = data;
  }
}
