package com.aiit.skyflying.common.service;

import com.aiit.skyflying.common.domain.SegmentDetailDo;
import com.aiit.skyflying.common.response.ServerResponse;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

public interface SegmentConsumerService extends ParentService<SegmentDetailDo, Long> {

    ServerResponse<String> consume(ConsumerRecord<String, Bytes> consumerRecord) throws Exception;

}
