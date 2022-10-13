package com.mingshi.skyflying.reactor.queue;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.reactor.thread.IoThread;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import com.mingshi.skyflying.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @ClassName OperatorRedisFailure
 * @Description @Author apple
 * Date 2021/6/9 下午4:25
 * @Version 1.0
 **/
@Slf4j
public class IoThreadLinkedBlockingQueue {
    /**
     * ioThread线程的数量，优雅关机时，会用到；2022-10-08 15:44:50
     */
    private static AtomicInteger ioThreadGraceShutdown = null;

    /**
     * 阻塞队列中，元素的个数；2021-06-09 16:30:20
     */
    private static final Integer QUEUE_SIZE = Const.IO_THREAD_QUEUE_SIZE;
    private static volatile List<IoThread> linkedBlockingQueueList = new ArrayList<>();
    /**
     * 单例的个数不能大于1，否则就不是单例了；2021-06-23 10:49:00
     */
    private static volatile AtomicInteger SINGLE_CASE_COUNT = new AtomicInteger(Const.NUMBER_ZERO);
    private static volatile AtomicInteger INDEX = new AtomicInteger(Const.NUMBER_ZERO);

    /**
     * <B>方法名称：decrementIoThreadGraceShutdown</B>
     * <B>概要说明：退出的线程数量减一</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 15:10:38
     * @Param []
     **/
    public static void decrementIoThreadGraceShutdown() {
        if (null != ioThreadGraceShutdown) {
            log.error("# InitProcessorByLinkedBlockingQueue.decrementProcessorGraceShutdown() # processor线程 = 【{}】要退出了。", Thread.currentThread().getName());
            ioThreadGraceShutdown.decrementAndGet();
        }
    }

    /**
     * <B>方法名称：getIoThreadGraceShutdown</B>
     * <B>概要说明：获取还剩下多少线程没有退出</B>
     *
     * @return java.lang.Integer
     * @Author zm
     * @Date 2022年10月08日 15:10:10
     * @Param []
     **/
    public static Integer getIoThreadGraceShutdown() {
        if(null != ioThreadGraceShutdown){
            return ioThreadGraceShutdown.get();
        }else{
            log.error("#  InitProcessorByLinkedBlockingQueue.decrementProcessorGraceShutdown()  # processor线程 = 【{}】要退出了，但 processorGraceShutdown 实例为null。");
            return -1;
        }
    }
    /**
     * 获取队列的容量；2021-11-17 14:35:19
     *
     * @return
     */
    public static Integer getQueueAllSize() {
        return QUEUE_SIZE;
    }

    public static List<IoThread> getLinkedBlockingQueueList() {
        return linkedBlockingQueueList;
    }

    /**
     * 私有构造函数，只能产生一个单例；2021-06-23 10:49:23
     *
     * @param localStatisticsThreadCount
     * @param mingshiServerUtil
     */
    private IoThreadLinkedBlockingQueue(Integer localStatisticsThreadCount, MingshiServerUtil mingshiServerUtil) {
        log.info("开始执行方法OperatorRedisFailureBuffer（）。");
        if (0 < SINGLE_CASE_COUNT.get()) {
            log.error("# IoThreadLinkedBlockingQueue.IoThreadLinkedBlockingQueue() # 类OperatorRedisFailureBuffer的实例个数大于1了（【{}】），不允许再次创建实例。", SINGLE_CASE_COUNT);
            return;
        }
        SINGLE_CASE_COUNT.incrementAndGet();
        for (Integer i = 0; i < localStatisticsThreadCount; i++) {
            log.info("# IoThreadLinkedBlockingQueue.IoThreadLinkedBlockingQueue() # 开始创建第【{}】个IoThread线程。", (1 + i));
            IoThread ioThread = new IoThread(QUEUE_SIZE, mingshiServerUtil);
            ioThread.setName(Const.IO_THREAD + i);
            ioThread.start();
            linkedBlockingQueueList.add(ioThread);
        }
        ioThreadGraceShutdown = new AtomicInteger(localStatisticsThreadCount);
    }

    /**
     * <B>方法名称：initLinkedBlockingQueueList</B>
     * <B>概要说明：初始化IoThead线程和对应的队列</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月27日 09:09:50
     * @Param [gracefulShutdown, localStatisticsThreadCount, flushToRocketMqInterval, mingshiServerUtil]
     **/
    private static void initLinkedBlockingQueueList(Integer reactorIoThreadCount, MingshiServerUtil mingshiServerUtil) {
        if (null == linkedBlockingQueueList || linkedBlockingQueueList.isEmpty()) {
            synchronized (IoThreadLinkedBlockingQueue.class) {
                if (null == linkedBlockingQueueList || linkedBlockingQueueList.isEmpty()) {
                    log.info("获取单例LinkedBlockingQueue。");
                    new IoThreadLinkedBlockingQueue(reactorIoThreadCount, mingshiServerUtil);
                }
            }
        }
    }

    /**
     * <B>方法名称：getLinkedBlockingQueue</B>
     * <B>概要说明：使用非优雅关机的方式获取IoThread线程对应的队列</B>
     *
     * @return java.util.concurrent.LinkedBlockingQueue
     * @Author zm
     * @Date 2022年09月27日 09:09:37
     * @Param [gracefulShutdown, localStatisticsThreadCount, flushToRocketMqInterval, mingshiServerUtil]
     **/
    public static IoThread getLinkedBlockingQueue(Integer reactorIoThreadCount, MingshiServerUtil mingshiServerUtil) {
        try {
            initLinkedBlockingQueueList(reactorIoThreadCount, mingshiServerUtil);
            Integer index = indexFor(INDEX.incrementAndGet(), linkedBlockingQueueList.size());
            return linkedBlockingQueueList.get(index);
        } catch (Exception e) {
            log.error("# IoThreadLinkedBlockingQueue.getLinkedBlockingQueue() # 在非优雅关机的情况下，获取IoThread线程所属内存队列时，出现了异常。", e);
        }
        return null;
    }

    private static volatile Boolean TWO_POWER_FLAG = null;

    /**
     * <B>方法名称：indexFor</B>
     * <B>概要说明：采用按位与代替取模运算，从而提升性能</B>
     *
     * @return int
     * 注意：这里要确保只有一个地方能够调用这个方法。如果多个地方调用这个方法这里就不合适了，因为 变量 TWO_POWER_FLAG 只会被初始化一次。
     * @Author zm
     * @Date 2022年06月01日 09:06:57
     * @Param [h, length]
     **/
    private static int indexFor(int h, int length) {
        if (null == TWO_POWER_FLAG) {
            //
            if (true == ReactorUtil.isTwoPower(length)) {
                TWO_POWER_FLAG = true;
            } else {
                TWO_POWER_FLAG = false;
            }
        }
        if (!TWO_POWER_FLAG.equals(null) && true == TWO_POWER_FLAG) {
            // 使用按位与获取下标；2022-09-13 14:20:28
            return h & (length - 1);
        }
        // 当length不是2的幂次方的时候，使用取模获取下标。2022-09-13 14:21:07
        return h % length;
    }
}
