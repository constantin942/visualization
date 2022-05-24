package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.util.concurrent.TimeUnit;

/**
 * @ClassName MboleUtil
 * @Description TODO
 * Author apple
 * Date 2021/1/5 下午3:14
 * @Version 1.0
 **/
@Slf4j
public class ReactorUtil {

  // 采用按位与代替取模运算；2021-12-24 10:18:52
  public static int indexFor(int h, int length) {
    return h & (length - 1);
  }

  public static void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> record) {
    try {
      while (false == InitProcessorByLinkedBlockingQueue.getCreateProcessorsFinishedFlag()) {
        log.error("processor线程还没有创建完毕，等待一会。");
        TimeUnit.SECONDS.sleep(50);
      }
    } catch (Exception e) {
      log.error("在消费者程序中，等待创建processor线程完毕时，出现了异常。", e);
    }
    try {
      boolean offerResult = false;
      while (false == offerResult) {
        ProcessorHandlerByLinkedBlockingQueue processorHandlerByLinkedBlockingQueue = InitProcessorByLinkedBlockingQueue.getProcessor();
        offerResult = processorHandlerByLinkedBlockingQueue.offer(record);
        if (false == offerResult) {
          log.info("所有的processor线程队列都满了，暂停10毫秒，接着再尝试。该processor线程中队列中的元素个数【{}】。", processorHandlerByLinkedBlockingQueue.getQueueSize());
          TimeUnit.MILLISECONDS.sleep(5000);
        }
      }
    } catch (Exception e) {
      log.error("消费者线程将拉取到的流量信息，分发给processor线程时，出现了异常。", e);
    }
  }
}
