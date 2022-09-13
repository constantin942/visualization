package com.mingshi.skyflying.disruptor.processor;

import com.lmax.disruptor.EventHandler;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Alienware
 */
@Slf4j
public class ProcessorConsumerByEventHandler implements EventHandler<SegmentByByte> {

  private SegmentConsumerService segmentConsumerService;

  public ProcessorConsumerByEventHandler(SegmentConsumerService segmentConsumerService) {
    this.segmentConsumerService = segmentConsumerService;
  }

  /**
   * 在单消费者的情况下使用。2021-12-23 07:53:11
   * @param segmentByByte
   * @param l
   * @param b
   * @throws Exception
   */
  @Override
  public void onEvent(SegmentByByte segmentByByte, long l, boolean b) throws Exception {
    try {
      segmentConsumerService.consumeByDisruptor(segmentByByte,true);
    } catch (Exception e) {
      log.error("# ProcessorConsumerByEventHandler.onEvent() # 出现了异常。", e);
    }
  }
}
