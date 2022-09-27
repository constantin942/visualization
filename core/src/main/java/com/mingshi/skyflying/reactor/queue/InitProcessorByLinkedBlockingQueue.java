package com.mingshi.skyflying.reactor.queue;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.ReactorUtil;
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

  /**
   * 是否开启reactor模式的开关；2022-06-01 09:28:28
   */
  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorByDisruptor;

  @Resource
  private SegmentConsumerService segmentConsumerService;

  private static Integer processorSize;
  private static AtomicInteger indexAtomicInteger = null;
  private static volatile Boolean createProcessorsFinishedFlag = false;
  private static List<ProcessorHandlerByLinkedBlockingQueue> processorHandlerByLinkedBlockingQueueList = null;

  public static Boolean getCreateProcessorsFinishedFlag() {
    return createProcessorsFinishedFlag;
  }

  private static Boolean shutdown = false;

  public static Boolean getShutdown() {
    return shutdown;
  }

  public static List<ProcessorHandlerByLinkedBlockingQueue> getProcessorHandlerByLinkedBlockingQueueList() {
    return processorHandlerByLinkedBlockingQueueList;
  }

  public static Integer getProcessorSize(){
    return processorSize;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    doRun();
  }

  private void doRun() {
    if (true == reactorProcessorEnable && false == reactorProcessorByDisruptor) {
      if (null == reactorProcessorThreadCount || Const.NUMBER_ZERO > reactorProcessorThreadCount) {
        // 在开启reactor模式的情况下，如果配置文件中没有设置创建processor线程的数量，那么默认设置为4；2022-06-01 09:32:11
        reactorProcessorThreadCount = Const.INITIAL_PROCESSOR_THREAD_COUNT;
      }
      processorSize = reactorProcessorThreadCount;
      indexAtomicInteger = new AtomicInteger(Const.NUMBER_ZERO);
      processorHandlerByLinkedBlockingQueueList = new ArrayList<>(reactorProcessorThreadCount);
      // 项目启动成功后，创建指定数量的processor线程；
      createProcessors();
      createProcessorsFinishedFlag = true;
    }
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
    for (int i = 0; i < processorSize; i++) {
      log.info("项目启动，开始创建第【{}】个processor线程，processor线程总数【{}】个。", (1 + i), processorSize);
      ProcessorHandlerByLinkedBlockingQueue processorHandlerByLinkedBlockingQueue = new ProcessorHandlerByLinkedBlockingQueue(segmentConsumerService);
      Thread thread = new Thread(processorHandlerByLinkedBlockingQueue);
      thread.setName("processor_" + i);
      thread.start();
      processorHandlerByLinkedBlockingQueueList.add(processorHandlerByLinkedBlockingQueue);
    }
  }

  /**
   * <B>方法名称：getProcessor</B>
   * <B>概要说明：获取processor线程</B>
   *
   * @return com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue
   * @Author zm
   * @Date 2022年06月01日 09:06:45
   * @Param []
   **/
  public static ProcessorHandlerByLinkedBlockingQueue getProcessor(Integer partition) {
    int index = ReactorUtil.indexFor(partition, processorSize);
    return processorHandlerByLinkedBlockingQueueList.get(index);
  }

  /**
   * <B>方法名称：getIndex</B>
   * <B>概要说明：获取索引</B>
   * @Author zm
   * @Date 2022年09月14日 23:09:37
   * @Param [partition]
   * @return java.lang.Integer
   **/
  public static Integer getIndex(Integer partition){
    return ReactorUtil.indexFor(partition, processorSize);
  }

  /**
   * <B>方法名称：getProcessor</B>
   * <B>概要说明：以轮询的方式获取ProcessorHandlerByLinkedBlockingQueue队列实例</B>
   * @Author zm
   * @Date 2022年09月27日 09:09:26
   * @Param []
   * @return com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue
   **/
  public static ProcessorHandlerByLinkedBlockingQueue getProcessor() {
    int index = ReactorUtil.indexFor(indexAtomicInteger.incrementAndGet(), processorSize);
    return processorHandlerByLinkedBlockingQueueList.get(index);
  }

}
