package com.mingshi.skyflying.utils;

import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.thread.ProcessorThread;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

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
    @Resource
    private SegmentConsumerService segmentConsumerService;

    private AtomicInteger countPrintLog = new AtomicInteger(0);

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

    public void doOnMessage(ConsumerRecord<String, Bytes> consumerRecord) {
        if (Boolean.TRUE.equals(reactorProcessorEnable)) {
            // 使用Reactor模式；
            // 使用LinkedBlockingQueue两把锁队列；2022-07-22 20:57:19
            useReactorModelByLinkedBlockingQueue(consumerRecord);
        } else {
            // 不使用Reactor模式；
            useNoReactorModel(consumerRecord);
        }
    }

    /**
     * <B>方法名称：useReactorModelByLinkedBlockingQueueByGracefulShutdown</B>
     * <B>概要说明：不启用优雅关机的方式</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月27日 09:09:28
     * @Param [consumerRecord]
     **/
    private void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> consumerRecord) {
        // 等待创建processor线程；2022-06-01 09:20:19
        waitingCreateProcessorsThread();
        try {
            ProcessorThread processorThread = null;
            boolean offerResult = false;
            while (false == offerResult) {
                processorThread = InitProcessorByLinkedBlockingQueue.getProcessor();
                offerResult = processorThread.offer(consumerRecord);
                // TimeUnit.MILLISECONDS.sleep(10);
                // if (false == offerResult) {
                //     if (100 * 10000 == countPrintLog.incrementAndGet()) {
                //         log.info("# AiitKafkaConsumerUtil.useReactorModelByLinkedBlockingQueue() # 消息对应的processor线程【{}】队列都满了，该processor线程中队列中的元素个数【{}】。",processorThread.getName(), processorThread.getQueueSize());
                //         countPrintLog.set(0);
                //     }
                // }
            }
        } catch (Throwable e) {
            log.error("# AiitKafkaConsumerUtil.useReactorModelByLinkedBlockingQueue() # 消费者线程将拉取到的流量信息分发给processor线程时，出现了异常。", e);
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
