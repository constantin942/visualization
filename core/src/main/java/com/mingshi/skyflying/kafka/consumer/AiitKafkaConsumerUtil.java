package com.mingshi.skyflying.kafka.consumer;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.disruptor.processor.ProcessorByDisruptor;
import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.clients.consumer.OffsetAndMetadata;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: AiitKafkaConsumerUtil</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * Date 2022/7/28 17:02
 * @Version 1.0
 **/
@Slf4j
@Component
public class AiitKafkaConsumerUtil {

  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorByDisruptor;

  @Resource
  private ProcessorByDisruptor processorByDisruptor;
  @Resource
  private SegmentConsumerService segmentConsumerService;
  @Resource
  private RedisPoolUtil redisPoolUtil;

  /**
   * <B>方法名称：commitOffsetSync</B>
   * <B>概要说明：提交offset</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年09月26日 08:09:03
   * @Param [aiitKafkaConsumer, offsetsLinkedBlockingQueue, count, start, timeMillis]
   **/
  public void commitOffsetSync(KafkaConsumer<String, Bytes> aiitKafkaConsumer,
                               LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> offsetsLinkedBlockingQueue,
                               int count,
                               Instant start) {
    try {
      Long timeMillis = DateTimeUtil.getTimeMillis(Instant.now());
      if (null == start) {
        start = Instant.now();
      }
      Map<TopicPartition, OffsetAndMetadata> offsetsMap = offsetsLinkedBlockingQueue.poll();
      if (null != offsetsMap && 0 < offsetsMap.size()) {
        // 测试是否重复消费的代码，已测试完毕。可以注销掉了。出现重复消费的场景是：Kafka服务端进行了rebalance。
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
    } catch (Exception e) {
      log.error("# AiitKafkaConsumerUtil.commitOffsetSync() # 提交offset到Kafka服务端时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：useReactorModelByDisruptor</B>
   * <B>概要说明：使用Disruptor无锁高性能队列</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月22日 20:07:28
   * @Param [record]
   **/
  public void useReactorModelByDisruptor(ConsumerRecord<String, Bytes> consumerRecord) {
    try {
      while (Boolean.FALSE.equals(processorByDisruptor.getCreateProcessorsFinishedFlag())) {
        log.error("Disruptor还没有创建完毕，等待一会。");
        TimeUnit.SECONDS.sleep(50);
      }
    } catch (Exception e) {
      log.error("在等待创建Disruptor完毕时，出现了异常。", e);
    }
    processorByDisruptor.offer(consumerRecord);
  }

  /**
   * <B>方法名称：useNoReactorModel</B>
   * <B>概要说明：使用非reactor模式清洗调用链信息</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月19日 17:05:03
   * @Param [record]
   **/
  public void useNoReactorModel(ConsumerRecord<String, Bytes> consumerRecord) {
    try {
      segmentConsumerService.consume(consumerRecord, false);
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
  }

  public void doOnMessage(ConsumerRecord<String, Bytes> consumerRecord,
                          Instant start,
                          KafkaConsumer<String, Bytes> aiitKafkaConsumer,
                          LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> offsetsLinkedBlockingQueue,
                          Integer count) {
    if (Boolean.TRUE.equals(reactorProcessorEnable)) {
      // 使用Reactor模式；
      // if (Boolean.TRUE.equals(reactorProcessorByDisruptor)) {
      //   // 使用Disruptor无锁高性能队列；2022-07-22 20:57:02
      //   // 在这里不能使用Disruptor了，因为生产者往队列里放消息时，是阻塞模式。我们这里为了提升性能必须使用非阻塞模式，所以在这里弃用Disruptor队列，只使用LinkedBlockingQueue队列。
      //   useReactorModelByDisruptor(consumerRecord);
      // } else {
      //   // 使用LinkedBlockingQueue两把锁队列；2022-07-22 20:57:19
      //   useReactorModelByLinkedBlockingQueue(consumerRecord);
      // }
      useReactorModelByLinkedBlockingQueue(consumerRecord, start, aiitKafkaConsumer, offsetsLinkedBlockingQueue, count);
    } else {
      // 不使用Reactor模式；
      useNoReactorModel(consumerRecord);
    }
  }

  /**
   * <B>方法名称：putRecordIntoBlockingQueue</B>
   * <B>概要说明：将拉取到的消息放入到processor线程对应的队列里</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 09:06:33
   * @Param [record]
   **/
  private void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> consumerRecord, Instant start, KafkaConsumer<String, Bytes> aiitKafkaConsumer,LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> offsetsLinkedBlockingQueue, Integer count) {
    // 等待创建processor线程；2022-06-01 09:20:19
    waitingCreateProcessorsThread();
    try {
      ProcessorHandlerByLinkedBlockingQueue processorHandlerByLinkedBlockingQueue = null;
      boolean offerResult = false;
      // 获取消息所属的partition，根据partition决定将消息放入到那个LinkedBlockingQueue中。 2022-09-13 13:54:23
      Integer partition = consumerRecord.partition();
      if (null != partition && 0 <= partition) {
        /** 使用put方法的优点：当前项目所在的服务器突然断电，或者是使用kill -9的方式关闭当前项目，此时在processor线程对应的内存队列中的消息和IoThread线程对应的内存队列里的消息会丢失。
         不过这些消息的offset不会提交到Kafka服务端，只有已消费成功的消息才会提交Kafka服务端。
         使用put方法的缺点：性能低，因为processor线程有可能会出现倾斜的情况。
         比如，当前消费者只消费某一个partition中的消息，那么这个partition中的消息只会路由到某一个processor线程中。此时这个processor线程有可能会一直处于繁忙的状态，其他的processor线程有可能处于空闲状态；
         当用Disruptor队列时，就不需要使用分区获取队列的下标了，因为只有一个Disruptor队列，在这个队列的后面挂着多个消费者线程。
         使用Disruptor队列性能会高很多。因为使用多个processor线程时出现了倾斜问题，在Disruptor中不存在，因为只有一个Disruptor队列，在这个队列的后面挂着多个消费者线程。
         如果只使用一个LinkedBlockingQueue阻塞队列，在这个阻塞队列的后面挂着多个线程，这种架构与使用Disruptor内存队列时是一样的。因为Disruptor内存队列是无锁的，而LinkedBlockingQueue
         是有锁的，理论上，使用Disruptor内存队列性能会高一些。不过还需要进行压测得到答案。2022-09-13 15:01:54
         如果非得坚持使用多个LinkedBlockingQueue内存队列，且每个LinkedBlockingQueue内存队列后面都挂着一个线程，此时如果要尽量避免processor线程出现倾斜，那么需要多方配合才行。
         比如：
         1. 创建topic时，尽量设置多个partition；
         2. 生产者将消息尽量均匀的发送到各个partition中；
         2. 消费者的个数不宜过多。比如，某个topic有16个partition，启动了4个消费者，每个消费者又启动了4个processor线程，此时一个processor线程就只能消费一个partition中的消息了。
         如果再有processor线程，那么这些多余的processor线程就会处于空闲的状态。
         使用这种架构（多个processor线程和多个LinkedBlockingQueue内存队列），限制条件比较多，如果partition的个数、启动消费者的个数、每个消费者中启动的processor线程的数据量不能合理设置，那么性能无法发挥到最大。
         推荐使用的架构：
         1. 使用一个Disruptor（推荐使用，因为是无锁的）或者LinkedBlockingQueue（后面的多个消费者线程会竞争出队列的锁，有性能损耗）内存队列，这个队列的后面挂着多个消费者线程； 2022-09-13 15:13:01
         **/
        // Integer index = InitProcessorByLinkedBlockingQueue.getIndex(partition);
        // 这一行代码会拖慢acceptor线程的速度；
        // redisPoolUtil.hsetIncrBy("index-" + index, "partition-" + partition, 1L);
        while (false == offerResult) {
          /** 使用offer方法的优点：性能高，processor线程不会出现倾斜的情况。因为每个processor线程处理的消息数量是差不多的。
           使用offer方法的缺点：整个链路在某些场景下，会丢失消息。
           某些场景比如是：当前项目所在的服务器突然断电，或者是使用kill -9的方式关闭当前项目，此时在processor线程对应的内存队列中的消息和IoThread线程对应的内存队列里的消息都会丢失。
           这些消息有可能已被提交offset，如果这些未消费的消息真的已提交offset到Kafka服务端，那么这些消息就真的丢失了。
           **/
          processorHandlerByLinkedBlockingQueue = InitProcessorByLinkedBlockingQueue.getProcessor(partition);
          offerResult = processorHandlerByLinkedBlockingQueue.offer(consumerRecord);
          if (false == offerResult) {
            log.info("消息对应的processor线程队列都满了，利用这个空隙，提交offset。该processor线程中队列中的元素个数【{}】。", processorHandlerByLinkedBlockingQueue.getQueueSize());
            commitOffsetSync(aiitKafkaConsumer, offsetsLinkedBlockingQueue, count, start);
          }
        }
      } else {
        log.error("# AiitKafkaConsumerUtil.useReactorModelByLinkedBlockingQueue # 根据消息的partition进行路由到processor线程对应的额内存队列里时，该消息没有partition字段值。这是不正常的。");
      }
    } catch (Throwable e) {
      log.error("# ReactorUtil.putRecordIntoBlockingQueue() # 消费者线程将拉取到的流量信息分发给processor线程时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：waitingCreateProcessorsThread</B>
   * <B>概要说明：等待创建processor线程</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 09:06:29
   * @Param []
   **/
  private static void waitingCreateProcessorsThread() {
    try {
      while (false == InitProcessorByLinkedBlockingQueue.getCreateProcessorsFinishedFlag()) {
        log.error("processor线程还没有创建完毕，等待一会。");
        TimeUnit.SECONDS.sleep(50);
      }
    } catch (Exception e) {
      log.error("在消费者程序中，等待创建processor线程完毕时，出现了异常。", e);
    }
  }
}
