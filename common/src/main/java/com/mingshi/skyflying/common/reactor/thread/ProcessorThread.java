package com.mingshi.skyflying.common.reactor.thread;

import com.mingshi.skyflying.common.config.GracefulShutdown;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.common.service.SegmentConsumerService;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.time.Instant;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

@Slf4j
public class ProcessorThread extends Thread {

    /**
     * 这里使用LinkedBlockingQueue的原因是：该阻塞队列有两把独占锁，分别是入队列的独占锁和出队列的独占锁。
     * 当线程执行入队列操作时，不影响操作出队列的线程。也就是说，执行入队列的线程与执行出队列的线程互不影响。2022-06-01 09:47:30
     */
    private LinkedBlockingQueue<ConsumerRecord<String, Bytes>> processorLinkedBlockingQueue;
    /**
     * 队列里存放的消息的个数；2022-06-01 09:42:19
     */
    private static final Integer QUEUE_SIZE = Const.QUEUE_SIZE;

    private Instant now = Instant.now();
    private SegmentConsumerService segmentConsumerService;

    public ProcessorThread(SegmentConsumerService segmentConsumerService) {
        this.segmentConsumerService = segmentConsumerService;
        this.processorLinkedBlockingQueue = new LinkedBlockingQueue<>(QUEUE_SIZE);
    }

    /**
     * <B>方法名称：offer</B>
     * <B>概要说明：往Processor线程自己的队列中放入数据</B>
     *
     * @return boolean
     * @Author zm
     * @Date 2022年09月27日 14:09:23
     * @Param [consumerRecord]
     **/
    public boolean offer(ConsumerRecord<String, Bytes> consumerRecord) {
        try {
            // 这里之所以使用阻塞队列的offer方法，是为了提升性能，提升性能的点：当队列满时，在不加锁的情况下，直接返回false。2022-06-01 09:44:53
            return processorLinkedBlockingQueue.offer(consumerRecord);
        } catch (Exception e) {
            log.error("将调用链信息(record)放入到LinkedBlockingQueue中出现了异常。", e);
            return false;
        }
    }

    /**
     * 获取队列中元素的个数；2021-10-20 15:22:55
     *
     * @return
     */
    public Integer getQueueSize() {
        return processorLinkedBlockingQueue.size();
    }

    @Override
    public void run() {
        Integer queueSize = 0;
        try {
            while (Boolean.TRUE.equals(GracefulShutdown.getRunning())) {
                doRun(Boolean.FALSE);
            }
            queueSize = getQueueSize();
            log.error("# ProcessorHandlerByLinkedBlockingQueue.run() # processor线程 = 【{}】要退出了。此时jvm关闭的标志位 = 【{}】，还没有执行finally代码块之前，线程对应的队列中元素的个数 = 【{}】。", Thread.currentThread().getName(), GracefulShutdown.getRunning(), queueSize);
        } finally {
            Instant now = Instant.now();
            try {
                while (!processorLinkedBlockingQueue.isEmpty()) {
                    doRun(Boolean.TRUE);
                }
            } finally {
                // 存活的processor线程数量减一；2022-10-08 15:19:34
                InitProcessorByLinkedBlockingQueue.decrementProcessorGraceShutdown();
                log.error("# ProcessorHandlerByLinkedBlockingQueue.run() # processor线程 = 【{}】要退出了。该线程对应的队列中元素的个数 = 【{}】。处理完【{}】条消息用时【{}】毫秒。",
                    Thread.currentThread().getName(),
                    getQueueSize(),
                    queueSize,
                    DateTimeUtil.getTimeMillis(now));
            }
        }
    }

    /**
     * <B>方法名称：doRun</B>
     * <B>概要说明：从内存队列里获取消息并消费</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 14:10:54
     * @Param []
     **/
    private void doRun(Boolean shutdown) {
        try {
            ConsumerRecord<String, Bytes> consumerRecord = processorLinkedBlockingQueue.poll();
            if (null == consumerRecord) {
                if (Const.NUMBER_TEN <= DateTimeUtil.getSecond(now)) {
                    // 提升性能：当队列为空的时候，每10秒打印一次日志。2022-06-01 09:50:19
                    log.info("当前 processor 线程【{}】对应的队列为空，休眠50毫秒。再尝试从队列里获取数据。", Thread.currentThread().getName());
                    now = Instant.now();
                }
                if (Boolean.FALSE.equals(shutdown)) {
                    TimeUnit.MILLISECONDS.sleep(Const.SLEEP_INTERVAL);
                }
            } else {
                segmentConsumerService.consume(consumerRecord);
            }
        } catch (Throwable e) {
            log.error("线程【{}】在清洗调用链信息时，出现了异常。", e);
        }
    }
}
