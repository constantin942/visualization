package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.common.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRebalanceListener;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.utils.Bytes;

import java.time.Instant;
import java.util.Collection;

/**
 * <B>主类名称: MsConsumerRebalanceListener</B>
 * <B>概要说明：发生再平衡时，同步提交当前消费者所负责分区的offset</B>
 * Author zm
 * Date 2022/10/9 13:49
 *
 * @Version 1.0
 **/
@Slf4j
public class MsConsumerRebalanceListener implements ConsumerRebalanceListener {

    private KafkaConsumer<String, Bytes> aiitKafkaConsumer = null;

    public MsConsumerRebalanceListener(KafkaConsumer<String, Bytes> aiitKafkaConsumer) {
        this.aiitKafkaConsumer = aiitKafkaConsumer;
    }

    /**
     * <B>方法名称：onPartitionsRevoked</B>
     * <B>概要说明：这个方法会在再平衡之前和消费者停止读取消息之后被调用。
     * 可以通过这个回调方法来处理消费位移的提交，以此来避免一些不必要的重复消费现象的发生。
     * 参数partitions表示再平衡前分配到的分区。
     * </B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月09日 13:10:51
     * @Param [partitions]
     **/
    @Override
    public void onPartitionsRevoked(Collection<TopicPartition> partitions) {
        Instant now = Instant.now();
        log.error("# MsKafkaSegmentsConsumer.init() # 要发生再平衡了，开始同步提交offset。");
        aiitKafkaConsumer.commitSync();
        log.error("# MsKafkaSegmentsConsumer.init() # 要发生再平衡了，完成同步提交offset，耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));

    }

    /**
     * <B>方法名称：onPartitionsAssigned</B>
     * <B>概要说明：这个方法会在重新分配分区之后和消费者开始读取消息之前被调用。参数partitions表示再平衡后分配到的分区。</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月09日 13:10:00
     * @Param [partitions]
     **/
    @Override
    public void onPartitionsAssigned(Collection<TopicPartition> partitions) {
        //do nothing
    }

}

