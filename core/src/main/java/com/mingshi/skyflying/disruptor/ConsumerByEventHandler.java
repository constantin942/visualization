package com.mingshi.skyflying.disruptor;

import com.lmax.disruptor.EventHandler;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Alienware
 */
@Slf4j
// @Component
public class ConsumerByEventHandler implements EventHandler<SegmentByByte> {

  private String applicationName;

  private SegmentConsumerService segmentConsumerService;

  public ConsumerByEventHandler(SegmentConsumerService segmentConsumerService,String applicationName) {
    this.segmentConsumerService = segmentConsumerService;
    this.applicationName = applicationName;
  }

  // 单线程的情况下使用；2021-12-23 07:53:11
  @Override
  public void onEvent(SegmentByByte data, long sequence, boolean endOfBatch) {
    try {
      segmentConsumerService.consumeByDisruptor(data,true);
    } catch (Exception e) {
      log.error("# ConsumerByEventHandler.onEvent() # 出现了异常。", e);
    }
  }

}
