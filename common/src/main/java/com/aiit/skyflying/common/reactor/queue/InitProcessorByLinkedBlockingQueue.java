package com.aiit.skyflying.common.reactor.queue;

import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.reactor.thread.ProcessorThread;
import com.aiit.skyflying.common.service.SegmentConsumerService;
import com.aiit.skyflying.common.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

@Slf4j
@Component
@PropertySource("classpath:application-${spring.profiles.active}.yml")
public class InitProcessorByLinkedBlockingQueue implements ApplicationRunner {

    /**
     * 在开启reactor模式的情况下，创建processor线程的数量；2022-06-01 09:28:57
     */
    @Value("${reactor.processor.thread.count}")
    private Integer reactorProcessorThreadCount;

    @Resource
    private SegmentConsumerService segmentConsumerService;

    /**
     * processor线程的数量，优雅关机时，会用到；2022-10-08 15:44:50
     */
    private static AtomicInteger processorGraceShutdown = null;

    private static Integer processorSize;
    private static AtomicInteger indexAtomicInteger = null;
    private static volatile Boolean createProcessorsFinishedFlag = false;
    private static List<ProcessorThread> processorThreadList = null;

    public static Boolean getCreateProcessorsFinishedFlag() {
        return createProcessorsFinishedFlag;
    }

    public static List<ProcessorThread> getProcessorHandlerByLinkedBlockingQueueList() {
        return processorThreadList;
    }

    public static Integer getProcessorSize() {
        return processorSize;
    }

    /**
     * <B>方法名称：decrementGraceShutdown</B>
     * <B>概要说明：退出的线程数量减一</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 15:10:38
     * @Param []
     **/
    public static void decrementProcessorGraceShutdown() {
        if (null != processorGraceShutdown) {
            log.error("# InitProcessorByLinkedBlockingQueue.decrementProcessorGraceShutdown() # processor线程 = 【{}】要退出了。", Thread.currentThread().getName());
            processorGraceShutdown.decrementAndGet();
        }
    }

    /**
     * <B>方法名称：getProcessorGraceShutdown</B>
     * <B>概要说明：获取还剩下多少线程没有退出</B>
     *
     * @return java.lang.Integer
     * @Author zm
     * @Date 2022年10月08日 15:10:10
     * @Param []
     **/
    public static Integer getProcessorGraceShutdown() {
        if (null != processorGraceShutdown) {
            return processorGraceShutdown.get();
        } else {
            log.error("#  InitProcessorByLinkedBlockingQueue.decrementProcessorGraceShutdown()  # processor线程 = 【{}】要退出了，但 processorGraceShutdown 实例为null。");
            return -1;
        }
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        doRun();
    }

    private void doRun() {
        if (null == reactorProcessorThreadCount || Const.NUMBER_ZERO > reactorProcessorThreadCount) {
            // 在开启reactor模式的情况下，如果配置文件中没有设置创建processor线程的数量，那么默认设置为4；2022-06-01 09:32:11
            reactorProcessorThreadCount = Const.INITIAL_PROCESSOR_THREAD_COUNT;
        }
        processorSize = reactorProcessorThreadCount;
        indexAtomicInteger = new AtomicInteger(Const.NUMBER_ZERO);
        processorThreadList = new ArrayList<>(reactorProcessorThreadCount);
        // 项目启动成功后，创建指定数量的processor线程；
        createProcessors();
        createProcessorsFinishedFlag = true;
        processorGraceShutdown = new AtomicInteger(processorSize);
    }

    /**
     * <B>方法名称：createProcessors</B>
     * <B>概要说明：项目启动成功后，创建指定数量的processor线程；</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年06月01日 09:06:24
     * @Param []
     **/
    private void createProcessors() {
        log.info("项目启动，开始创建【{}】个processor线程。", processorSize);
        for (int i = 0; i < processorSize; i++) {
            log.info("项目启动，开始创建第【{}】个processor线程，processor线程总数【{}】个。", (1 + i), processorSize);
            ProcessorThread processorThread = new ProcessorThread(segmentConsumerService);
            processorThread.setName(Const.PROCESSOR_THREAD + i);
            processorThread.start();
            processorThreadList.add(processorThread);
        }
    }

    /**
     * <B>方法名称：getProcessor</B>
     * <B>概要说明：以轮询的方式获取ProcessorHandlerByLinkedBlockingQueue队列实例</B>
     *
     * @return com.aiit.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue
     * @Author zm
     * @Date 2022年09月27日 09:09:26
     * @Param []
     **/
    public static ProcessorThread getProcessor() {
        if (Const.NUMBER_ONE.equals(processorThreadList.size())) {
            return processorThreadList.get(Const.NUMBER_ZERO);
        }
        int index = ReactorUtil.indexForByProcessor(indexAtomicInteger.incrementAndGet(), processorSize);
        return processorThreadList.get(index);
    }

}
