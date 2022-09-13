package com.mingshi.skyflying.kafka.consumer;

import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.OffsetAndMetadata;
import org.apache.kafka.clients.consumer.OffsetCommitCallback;
import org.apache.kafka.common.TopicPartition;

import java.util.Arrays;
import java.util.Map;

/**
 * <B>主类名称: AiitOffsetCommitCallback</B>
 * <B>概要说明：异步提交offset的回调函数</B>
 * Author zm
 * Date 2022/9/13 10:55
 *
 * @Version 1.0
 **/
@Slf4j
public class AiitOffsetCommitCallback implements OffsetCommitCallback {
  @Override
  public void onComplete(Map<TopicPartition, OffsetAndMetadata> offsets, Exception exception) {
    if (exception != null) {
      log.error("# ConsumerTest.run() # 手动异步提交Kafka的offset = 【{}】失败，其异常信息是【{}】。", offsets, Arrays.toString(exception.getStackTrace()));
    }
  }
}
