package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.util.concurrent.TimeUnit;

/**
 * @ClassName MboleUtil
 * @Author apple
 * Date 2021/1/5 下午3:14
 * @Version 1.0
 **/
@Slf4j
public class ReactorUtil {
  private static volatile Boolean TWO_POWER_FLAG = null;

  /**
   * <B>方法名称：indexFor</B>
   * <B>概要说明：采用按位与代替取模运算，从而提升性能</B>
   * @Author zm
   * @Date 2022年06月01日 09:06:57
   * @Param [h, length]
   * @return int
   * 注意：这里要确保只有一个地方能够调用这个方法。如果多个地方调用这个方法这里就不合适了，因为 变量 TWO_POWER_FLAG 只会被初始化一次。
   **/
  public static int indexFor(int h, int length) {
    if(null == TWO_POWER_FLAG){
      //
      if(true == isTwoPower(length)){
        TWO_POWER_FLAG = true;
      }else{
        TWO_POWER_FLAG = false;
      }
    }
    if(!TWO_POWER_FLAG.equals(null) && true  == TWO_POWER_FLAG){
      // 使用按位与获取下标；2022-09-13 14:20:28
      return h & (length - 1);
    }
    // 当length不是2的幂次方的时候，使用取模获取下标。2022-09-13 14:21:07
    return h % length;
  }

  /**
   * <B>方法名称：isTwoPower</B>
   * <B>概要说明：判断是不是2的幂次方</B>
   * @Author zm
   * @Date 2022年09月13日 14:09:43
   * @Param [n]
   * @return boolean
   **/
  public static boolean isTwoPower(int n) {
    return n>0 && (n&(n-1)) == 0;
  }

  public static void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> consumerRecord) {
    // 将拉取到的消息放入到processor线程对应的队列里；2022-06-01 09:24:47
    putRecordIntoBlockingQueue(consumerRecord);
  }

  /**
   * <B>方法名称：putRecordIntoBlockingQueue</B>
   * <B>概要说明：将拉取到的消息放入到processor线程对应的队列里</B>
   * @Author zm
   * @Date 2022年06月01日 09:06:33
   * @Param [record]
   * @return void
   **/
  private static void putRecordIntoBlockingQueue(ConsumerRecord<String, Bytes> consumerRecord) {
    // 等待创建processor线程；2022-06-01 09:20:19
    waitingCreateProcessorsThread();
    try {
      ProcessorHandlerByLinkedBlockingQueue processorHandlerByLinkedBlockingQueue = null;
      boolean offerResult = false;
      // 获取消息所属的partition，根据partition决定将消息放入到那个LinkedBlockingQueue中。 2022-09-13 13:54:23
      Integer partition = consumerRecord.partition();
      if(null != partition && 0 < partition){
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
        processorHandlerByLinkedBlockingQueue = InitProcessorByLinkedBlockingQueue.getProcessor(partition);
        processorHandlerByLinkedBlockingQueue.put(consumerRecord);
      }else {
        while (false == offerResult) {
          /** 使用offer方法的优点：性能高，processor线程不会出现倾斜的情况。因为每个processor线程处理的消息数量是差不多的。
           使用offer方法的缺点：整个链路在某些场景下，会丢失消息。
                             某些场景比如是：当前项目所在的服务器突然断电，或者是使用kill -9的方式关闭当前项目，此时在processor线程对应的内存队列中的消息和IoThread线程对应的内存队列里的消息都会丢失。
                             这些消息有可能已被提交offset，如果这些未消费的消息真的已提交offset到Kafka服务端，那么这些消息就真的丢失了。
           **/
          processorHandlerByLinkedBlockingQueue = InitProcessorByLinkedBlockingQueue.getProcessor();
          offerResult = processorHandlerByLinkedBlockingQueue.offer(consumerRecord);
          if (false == offerResult) {
            log.info("所有的processor线程队列都满了，暂停100毫秒，接着再尝试。该processor线程中队列中的元素个数【{}】。", processorHandlerByLinkedBlockingQueue.getQueueSize());
            TimeUnit.MILLISECONDS.sleep(100);
          }
        }
      }
    } catch (Throwable e) {
      log.error("# ReactorUtil.putRecordIntoBlockingQueue() # 消费者线程将拉取到的流量信息分发给processor线程时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：waitingCreateProcessorsThread</B>
   * <B>概要说明：等待创建processor线程</B>
   * @Author zm
   * @Date 2022年06月01日 09:06:29
   * @Param []
   * @return void
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
