package com.mingshi.skyflying.disruptor;

import com.lmax.disruptor.EventFactory;

/**
 * @ClassName FlowsMatchFactory
 * @Description TODO
 * Author apple
 * Date 2021/11/16 07:59
 * @Version 1.0
 **/
public class ConsumerFactory implements EventFactory {
  @Override
  public SegmentByByte newInstance() {
    return new SegmentByByte();
  }
}
