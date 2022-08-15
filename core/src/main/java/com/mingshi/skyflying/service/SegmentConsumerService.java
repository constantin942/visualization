package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.SegmentDetailDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.disruptor.processor.SegmentByByte;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

public interface SegmentConsumerService extends ParentService<SegmentDetailDo, Long> {

  ServerResponse<String> consume(ConsumerRecord<String, Bytes> msg, Boolean enableReactorModelFlag);

  ServerResponse<String> consumeByDisruptor(SegmentByByte segmentByByte, Boolean enableReactorModelFlag);

}
