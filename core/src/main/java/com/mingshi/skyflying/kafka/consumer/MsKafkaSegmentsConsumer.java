package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.*;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.serialization.BytesDeserializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.utils.Bytes;

import java.time.Duration;
import java.time.Instant;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.LinkedBlockingQueue;
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
  private boolean reactorProcessorEnable = false;
  private boolean gracefulShutdown = false;
  private LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> offsetsLinkedBlockingQueue;
  private RedisPoolUtil redisPoolUtil;

  /**
   * 初始化完成的标志；2022-07-28 17:12:32
   */
  private volatile AtomicBoolean isInitDone = new AtomicBoolean(false);

  private KafkaConsumer<String, Bytes> aiitKafkaConsumer = null;

  public MsKafkaSegmentsConsumer(RedisPoolUtil redisPoolUtil, LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> linkedBlockingQueue, boolean gracefulShutdown, boolean reactorProcessorEnable, AiitKafkaConsumerUtil aiitKafkaConsumerUtil, String bootstrapServers, String consumerTopic, String consumerGroup) {
    this.aiitKafkaConsumerUtil = aiitKafkaConsumerUtil;
    this.bootstrapServers = bootstrapServers;
    this.consumerTopic = consumerTopic;
    this.consumerGroup = consumerGroup;
    this.reactorProcessorEnable = reactorProcessorEnable;
    this.gracefulShutdown = gracefulShutdown;
    this.offsetsLinkedBlockingQueue = linkedBlockingQueue;
    this.redisPoolUtil = redisPoolUtil;
  }

  /**
   * 获取队列
   *
   * @return
   */
  public LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> getOffsetsLinkedBlockingQueue() {
    return offsetsLinkedBlockingQueue;
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
    properties.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, 300);
    // 如果两次poll操作间隔超过了这个时间，broker就会认为这个consumer处理能力太弱，会将其踢出消费组，将分区分配给别的consumer消费
    properties.put(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, 30 * 1000);
    // 把消息的key从字节数组反序列化为字符串
    properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    // 把消息的value从字节数组反序列化为字符串
    properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, BytesDeserializer.class.getName());

    aiitKafkaConsumer = new org.apache.kafka.clients.consumer.KafkaConsumer<>(properties);

    // 订阅主题
    aiitKafkaConsumer.subscribe(Collections.singletonList(consumerTopic));
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
    while (true) {
      try {
        ConsumerRecords<String, Bytes> records = aiitKafkaConsumer.poll(Duration.ofMillis(100));
        Instant start = Instant.now();
        int count = records.count();
        for (ConsumerRecord<String, Bytes> record : records) {
          aiitKafkaConsumerUtil.doOnMessage(record);
        }

        // 根据配置情况，提交offset
        commitAck(count, start);
      } catch (Exception e) {
        log.error(" # ConsumerTest.run() # 消费消息时，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：commitAck</B>
   * <B>概要说明：根据配置情况，提交offset</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年09月14日 16:09:59
   * @Param [count]
   **/
  private void commitAck(int count, Instant start) {
    long timeMillis = DateTimeUtil.getTimeMillis(start);
    if (count > 0) {
      if (false == reactorProcessorEnable) {
        // 在不启用reactor模型的情况下，每消费完一批消息，就同步提交这批消息的offset；2022-09-13 17:04:03
        aiitKafkaConsumer.commitSync();
        log.info("# MsKafkaSegmentsConsumer.commitAck() # 使用非reactor模式，当消息处理完毕之后，才提交这批消息的offset。本次KafkaConsumer线程从Kafka服务端拉取到【{}】条消息。把这些消息放入到第一层队列里用时【{}】毫秒。从第三层队列里获取到要提交的offset，并提交到Kafka服务端用时【{}】毫秒。", count, timeMillis, (DateTimeUtil.getTimeMillis(start) - timeMillis));
      } else if (false == gracefulShutdown) {
        // 不启用优雅关机；2022-09-13 17:13:10
        // 手动异步提交offset，当前线程提交offset不会阻塞，可以继续处理后面的程序逻辑
        aiitKafkaConsumer.commitAsync(new AiitOffsetCommitCallback());
      }
    }
    if (true == gracefulShutdown) {
      // 优雅关机；2022-09-13 17:38:32
      while (true) {
        Map<TopicPartition, OffsetAndMetadata> offsetsMap = offsetsLinkedBlockingQueue.poll();
        if (null != offsetsMap && 0 < offsetsMap.size()) {

          Iterator<TopicPartition> iterator = offsetsMap.keySet().iterator();
          while (iterator.hasNext()) {
            TopicPartition key = iterator.next();
            String topic = key.topic();
            int partition = key.partition();
            OffsetAndMetadata value = offsetsMap.get(key);
            long offset = value.offset();
            String item = topic + ":" + partition + ":" + offset;
            Object hget = redisPoolUtil.hget(Const.HASH_TEST_GRACEFUL_SHUTDOWN, item);
            if (null != hget) {
              log.error("根据 key = 【{}】，在表 = 【{}】中获取到了已提交过的offset = 【{}】。", item, Const.HASH_TEST_GRACEFUL_SHUTDOWN, offsetsMap);
            } else {
              redisPoolUtil.hset(Const.HASH_TEST_GRACEFUL_SHUTDOWN, item, "existed");
            }
          }

          // 优先提交offset；
          aiitKafkaConsumer.commitSync(offsetsMap);
          log.info("# MsKafkaSegmentsConsumer.commitAck() # 当消息处理完毕之后，才提交这批消息的offset = 【{}】。本次KafkaConsumer线程从Kafka服务端拉取到【{}】条消息。把这些消息放入到第一层队列里用时【{}】毫秒。从第三层队列里获取到要提交的offset，并提交到Kafka服务端用时【{}】毫秒。当前队列中元素的个数【{}】。",
            offsetsMap,
            count,
            timeMillis,
            (DateTimeUtil.getTimeMillis(start) - timeMillis),
            offsetsLinkedBlockingQueue.size());
          offsetsMap.clear();
        }
        break;
      }

    }
  }
}
