package com.mingshi.skyflying.common.reactor.queue;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.reactor.thread.IoThread;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import com.mingshi.skyflying.common.utils.ReactorUtil;
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
    private static AtomicInteger SINGLE_CASE_COUNT = new AtomicInteger(Const.NUMBER_ZERO);
    private static AtomicInteger INDEX = new AtomicInteger(Const.NUMBER_ZERO);

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
        if (null != ioThreadGraceShutdown) {
            return ioThreadGraceShutdown.get();
        } else {
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
        Integer index = null;
        try {
            initLinkedBlockingQueueList(reactorIoThreadCount, mingshiServerUtil);
            index =  ReactorUtil.indexForByIoThread(INDEX.incrementAndGet(), linkedBlockingQueueList.size());
            if(0 > index){
                index = Math.abs(index);
            }
            return linkedBlockingQueueList.get(index);
        } catch (Exception e) {
            log.error("# IoThreadLinkedBlockingQueue.getLinkedBlockingQueue() # 在非优雅关机的情况下，获取IoThread线程所属内存队列时，出现了异常。计算出来的下标索引 = 【{}】，当前队列的大小 = 【{}】", index, linkedBlockingQueueList.size(), e);
        }
        return null;
    }
}
