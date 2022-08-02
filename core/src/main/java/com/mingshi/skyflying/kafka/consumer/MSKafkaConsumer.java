package com.mingshi.skyflying.kafka.consumer;

import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.*;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.serialization.BytesDeserializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.utils.Bytes;

import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

@Slf4j
public class MSKafkaConsumer extends Thread {

  private AiitKafkaConsumerUtil aiitKafkaConsumerUtil;

  private String bootstrapServers;

  // private final static String TOPIC = "test-skywalking-segments";
  // private final static String GROUP = "test-skyflying-consumer-group";
  private final static String TOPIC = "skywalking-segments";
  private final static String GROUP = "skyflying-consumer-group";

  // 初始化完成的标志；2022-07-28 17:12:32
  private AtomicBoolean isInitDone = new AtomicBoolean(false);

  org.apache.kafka.clients.consumer.KafkaConsumer<String, Bytes> aiitKafkaConsumer = null;

  public MSKafkaConsumer(AiitKafkaConsumerUtil aiitKafkaConsumerUtil, String bootstrapServers) {
    this.aiitKafkaConsumerUtil = aiitKafkaConsumerUtil;
    this.bootstrapServers = bootstrapServers;
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
    properties.put(ConsumerConfig.GROUP_ID_CONFIG, GROUP);
    // 是否自动提交offset，默认就是true
//		properties.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true");
    // 自动提交offset的间隔时间, 每1000毫秒提交一次
//		properties.put(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "1000");
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
    properties.put(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, 30 * 1000);
    // 把消息的key从字节数组反序列化为字符串
    properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
    // 把消息的value从字节数组反序列化为字符串
    properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, BytesDeserializer.class.getName());

    aiitKafkaConsumer = new org.apache.kafka.clients.consumer.KafkaConsumer<>(properties);

    // 订阅主题
    aiitKafkaConsumer.subscribe(Collections.singletonList(TOPIC));
    isInitDone.set(true);
  }

  @Override
  public void run() {
    // init();
    // if (false == isInitDone.get()) {
    //   log.error("# ConsumerTest.run() # 初始化失kafka消费者败，不能消费kafka服务端的消息。");
    //   return;
    // }
    // doRun();
  }

  private void doRun() {
    // todo：这里需要设置优雅关机。2022-07-28 17:34:52
    while (true) {
      try {
        // Instant now = Instant.now();
        // poll(duration): 长轮询, 即duration时段内没拿到消息就一直重复尝试拿, 知道时间到或者拿到消息才返回结果
        ConsumerRecords<String, Bytes> records = aiitKafkaConsumer.poll(Duration.ofMillis(1000));
        int count = records.count();
        for (ConsumerRecord<String, Bytes> record : records) {
          aiitKafkaConsumerUtil.doOnMessage(record);
        }

        if (count > 0) {
          // log.error("# ConsumerTest.run() # 当前线程是 = 【{}】，kafka消费者线程拉取到了【{}条】消息，将这些消息放入到Processor线程对应的内存队列中耗时【{}】毫秒。", Thread.currentThread().getName(), count, DateTimeUtil.getTimeMillis(now));
          // 手动同步提交offset，当前线程会阻塞直到offset提交成功, 一般使用同步提交，因为提交之后一般也没有什么逻辑代码了
          // consumer.commitSync();

          // 手动异步提交offset，当前线程提交offset不会阻塞，可以继续处理后面的程序逻辑
          aiitKafkaConsumer.commitAsync(new OffsetCommitCallback() {
            @Override
            public void onComplete(Map<TopicPartition, OffsetAndMetadata> offsets, Exception exception) {
              if (exception != null) {
                log.error("# ConsumerTest.run() # 手动异步提交Kafka的offset = 【{}】失败，其异常信息是【{}】。", offsets, Arrays.toString(exception.getStackTrace()));
              }
            }
          });
        }
      } catch (Exception e) {
        log.error(" # ConsumerTest.run() # 消费消息时，出现了异常。", e);
      }
    }
  }
}
