package com.mingshi.skyflying.disruptor.processor;

import com.lmax.disruptor.EventFactory;

/**
 * @ClassName FlowsMatchFactory
 * @Description TODO
 * @Author apple
 * Date 2021/11/16 07:59
 * @Version 1.0
 **/
public class ConsumerProcessorFactory implements EventFactory {
  @Override
  public SegmentByByte newInstance() {
    return new SegmentByByte();
  }
}
