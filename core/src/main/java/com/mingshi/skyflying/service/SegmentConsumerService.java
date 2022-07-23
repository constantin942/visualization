package com.mingshi.skyflying.service;

import com.mingshi.skyflying.disruptor.SegmentByByte;
import com.mingshi.skyflying.domain.SegmentDetailDo;
import com.mingshi.skyflying.response.ServerResponse;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

public interface SegmentConsumerService extends ParentService<SegmentDetailDo, Long> {

  ServerResponse<String> consume(ConsumerRecord<String, Bytes> record, Boolean enableReactorModelFlag);

  ServerResponse<String> consumeByDisruptor(SegmentByByte record, Boolean enableReactorModelFlag);

}
