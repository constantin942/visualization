package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.common.config.GracefulShutdown;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.reactor.queue.IoThreadLinkedBlockingQueue;
import com.mingshi.skyflying.utils.AiitKafkaConsumerUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.BytesDeserializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.utils.Bytes;

import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>类名称：MsKafkaSegmentsConsumer</B>
 * <B>概要说明：消费segment的消费者</B>
 *
 * @Author zm
 * @Date 2022年08月25日 11:08:17
 * @Param
 * @return
 **/
@Slf4j
public class MsKafkaSegmentsConsumer extends Thread {

    private String consumerTopic;
    private String consumerGroup;
    private String bootstrapServers;
    private AiitKafkaConsumerUtil aiitKafkaConsumerUtil;

    /**
     * 初始化完成的标志；2022-07-28 17:12:32
     */
    private volatile AtomicBoolean isInitDone = new AtomicBoolean(false);

    private KafkaConsumer<String, Bytes> aiitKafkaConsumer = null;

    public MsKafkaSegmentsConsumer(AiitKafkaConsumerUtil aiitKafkaConsumerUtil, String bootstrapServers, String consumerTopic, String consumerGroup) {
        this.aiitKafkaConsumerUtil = aiitKafkaConsumerUtil;
        this.bootstrapServers = bootstrapServers;
        this.consumerTopic = consumerTopic;
        this.consumerGroup = consumerGroup;
    }

    /**
     * <B>方法名称：getIsInitDone</B>
     * <B>概要说明：消费者线程初始化完毕的标志</B>
     *
     * @return java.util.concurrent.atomic.AtomicBoolean
     * @Author zm
     * @Date 2022年09月13日 13:09:29
     * @Param []
     **/
    public Boolean getIsInitDone() {
        return isInitDone.get();
    }

    /**
     * <B>方法名称：init</B>
     * <B>概要说明：初始化消费者配置信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月28日 17:07:39
     * @Param []
     **/
    public void init() {
        Properties properties = new Properties();
        properties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);
        // 消费分组名
        properties.put(ConsumerConfig.GROUP_ID_CONFIG, consumerGroup);
        // 是否自动提交offset
        properties.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false");
        // 当消费主题的是一个新的消费组，或者指定offset的消费方式，offset不存在，那么应该如何消费
        // latest(默认) ：只消费自己启动之后才发送到主题的消息
        // earliest：第一次从头开始消费，以后按照消费offset记录继续消费，这个需要区别于consumer.seekToBeginning(每次都从头开始消费)
        properties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        // consumer给broker发送心跳的间隔时间，broker接收到心跳如果此时有rebalance发生会通过心跳响应将rebalance方案下发给consumer，这个时间可以稍微短一点
        properties.put(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, 1000);
        // broker接收不到一个consumer的心跳, 持续该时间, 就认为故障了，会将其踢出消费组，对应的Partition也会被重新分配给其他consumer，默认是10秒
        properties.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, 30 * 1000);
        // 一次poll最大拉取消息的条数，如果消费者处理速度很快，可以设置大点，如果处理速度一般，可以设置小点
        properties.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, 1000);
        // 如果两次poll操作间隔超过了这个时间，broker就会认为这个consumer处理能力太弱，会将其踢出消费组，将分区分配给别的consumer消费
        properties.put(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, 60 * 1000);
        // 把消息的key从字节数组反序列化为字符串
        properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        // 把消息的value从字节数组反序列化为字符串
        properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, BytesDeserializer.class.getName());

        aiitKafkaConsumer = new org.apache.kafka.clients.consumer.KafkaConsumer<>(properties);

        // 订阅主题
        aiitKafkaConsumer.subscribe(Collections.singletonList(consumerTopic), new MsConsumerRebalanceListener(aiitKafkaConsumer));
        isInitDone.set(true);
    }

    @Override
    public void run() {
        init();
        if (false == isInitDone.get()) {
            log.error("# ConsumerTest.run() # 初始化失kafka消费者败，不能消费kafka服务端的消息。");
            return;
        }
        doRun();
    }

    private void doRun() {
        try {
            while (Boolean.TRUE.equals(GracefulShutdown.getRUNNING())) {
                consumeRecords();
            }
        } finally {
            try {
                log.info("# MsKafkaSegmentsConsumer.doRun() # 优雅关机，开始同步提交offset。");
                doCommitSync();
                log.info("# MsKafkaSegmentsConsumer.doRun() # 优雅关机，同步提交offset完毕。");
            } finally {
                aiitKafkaConsumer.close();
            }
        }
    }

    /**
     * <B>方法名称：consumeRecords</B>
     * <B>概要说明：从Kafka服务端拉取消息并消费消息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 16:10:38
     * @Param []
     **/
    private void consumeRecords() {
        try {
            ConsumerRecords<String, Bytes> records = aiitKafkaConsumer.poll(Duration.ofMillis(200));
            Integer count = records.count();
            for (ConsumerRecord<String, Bytes> consumerRecord : records) {
                aiitKafkaConsumerUtil.doOnMessage(consumerRecord);
            }
            if (count > 0) {
                // 手动异步提交offset，当前线程提交offset不会阻塞，可以继续处理后面的程序逻辑
                aiitKafkaConsumer.commitAsync(new AiitOffsetCommitCallback());
            }
        } catch (Exception e) {
            log.error(" # ConsumerTest.run() # 消费消息时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：doCommitSync</B>
     * <B>概要说明：等待所有的ioThread线程都退出后，再提交offset</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 16:10:41
     * @Param []
     **/
    private void doCommitSync() {
        try {
            while (0 < IoThreadLinkedBlockingQueue.getIoThreadGraceShutdown()) {
                TimeUnit.MILLISECONDS.sleep(Const.NUM_ONE);
            }
            Instant now = Instant.now();
            log.error("# MsKafkaSegmentsConsumer.doCommitSync() # 优雅关机，所有的IoThread线程都已退出，此时再同步提交offset。");
            aiitKafkaConsumer.commitSync();
            log.error("# MsKafkaSegmentsConsumer.doCommitSync() # 优雅关机，所有的IoThread线程都已退出，此时再同步提交offset，用时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
        } catch (InterruptedException e) {
            log.error("# MsKafkaSegmentsConsumer.doCommitSync() # 优雅关机，同步提交offset时，出现了异常。", e);
        }
    }
}
