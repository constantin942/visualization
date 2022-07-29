package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.util.concurrent.TimeUnit;

/**
 * @ClassName MboleUtil
 * Author apple
 * Date 2021/1/5 下午3:14
 * @Version 1.0
 **/
@Slf4j
public class ReactorUtil {

  /**
   * <B>方法名称：indexFor</B>
   * <B>概要说明：采用按位与代替取模运算，从而提升性能</B>
   * @Author zm
   * @Date 2022年06月01日 09:06:57
   * @Param [h, length]
   * @return int
   **/
  public static int indexFor(int h, int length) {
    return h & (length - 1);
  }

  public static void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> record) {
    // 将拉取到的消息放入到processor线程对应的队列里；2022-06-01 09:24:47
    putRecordIntoBlockingQueue(record);
  }

  /**
   * <B>方法名称：putRecordIntoBlockingQueue</B>
   * <B>概要说明：将拉取到的消息放入到processor线程对应的队列里</B>
   * @Author zm
   * @Date 2022年06月01日 09:06:33
   * @Param [record]
   * @return void
   **/
  private static void putRecordIntoBlockingQueue(ConsumerRecord<String, Bytes> record) {
    // 等待创建processor线程；2022-06-01 09:20:19
    waitingCreateProcessorsThread();
    try {
      boolean offerResult = false;
      while (false == offerResult) {
        ProcessorHandlerByLinkedBlockingQueue processorHandlerByLinkedBlockingQueue = InitProcessorByLinkedBlockingQueue.getProcessor();
        offerResult = processorHandlerByLinkedBlockingQueue.offer(record);
        if (false == offerResult) {
          // log.info("所有的processor线程队列都满了，暂停100毫秒，接着再尝试。该processor线程中队列中的元素个数【{}】。", processorHandlerByLinkedBlockingQueue.getQueueSize());
          TimeUnit.MILLISECONDS.sleep(10);
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
