package com.mingshi.skyflying.disruptor.processor;

import com.lmax.disruptor.WorkHandler;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Alienware
 */
@Slf4j
public class ProcessorConsumerByWrokHandler implements WorkHandler<SegmentByByte> {

  private SegmentConsumerService segmentConsumerService;

  public ProcessorConsumerByWrokHandler(SegmentConsumerService segmentConsumerService) {
    this.segmentConsumerService = segmentConsumerService;
  }

  /**
   * 在多消费者的情况下使用。每个消费者只会消费其中一个消息。2021-12-23 07:53:11
   * @param segmentByByte
   */
  @Override
  public void onEvent(SegmentByByte segmentByByte) {
    try {
      segmentConsumerService.consumeByDisruptor(segmentByByte,true);
    } catch (Exception e) {
      log.error("# ConsumerByEventHandler.onEvent() # 出现了异常。", e);
    }
  }

}
