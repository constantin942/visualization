package com.mingshi.skyflying.reactor.queue;

import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.utils.ReactorUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

@Slf4j
@Component
public class InitProcessorByLinkedBlockingQueue implements ApplicationRunner {
  @Value("${reactor.processor.thread.count}")
  private Integer reactorProcessorThreadCount = 4;

  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Resource
  private SegmentConsumerService segmentConsumerService;

  private static volatile Boolean createProcessorsFinishedFlag = false;
  private static Integer processorSize = 1;
  private static List<ProcessorHandlerByLinkedBlockingQueue> processorHandlerByLinkedBlockingQueueList = new ArrayList<>(processorSize);

  private static AtomicInteger indexAtomicInteger = new AtomicInteger(0);

  public static Boolean getCreateProcessorsFinishedFlag() {
    return createProcessorsFinishedFlag;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    if(true == reactorProcessorEnable){
      // 项目启动成功后，创建指定数量的processor线程；
      createProcessors();
      createProcessorsFinishedFlag = true;
    }
  }

  // 项目启动成功后，创建指定数量的processor线程；
  private void createProcessors() {
    if(null == reactorProcessorThreadCount || 0 > reactorProcessorThreadCount){
      reactorProcessorThreadCount = 8;
    }
    processorSize = reactorProcessorThreadCount;
    for (int i = 0; i < processorSize; i++) {
      log.info("项目启动，开始创建第【{}】个processor线程，processor线程总数【{}】个。", (1 + i), processorSize);
      ProcessorHandlerByLinkedBlockingQueue processorHandlerByLinkedBlockingQueue = new ProcessorHandlerByLinkedBlockingQueue(segmentConsumerService);
      Thread thread = new Thread(processorHandlerByLinkedBlockingQueue);
      thread.setName("processor_" + i);
      thread.start();
      processorHandlerByLinkedBlockingQueueList.add(processorHandlerByLinkedBlockingQueue);
    }
  }

  // 获取processor线程；
  public static ProcessorHandlerByLinkedBlockingQueue getProcessor() {
    // 当自增的原子类实例自增到processor线程数量时，就重置为0；2021-10-20 14:26:25
    if (indexAtomicInteger.get() == processorSize) {
      indexAtomicInteger = new AtomicInteger(0);
    }

    int index = ReactorUtil.indexFor(indexAtomicInteger.incrementAndGet(), processorSize);
    // log.info("本次获取到processor线程的索引值 = 【{}】，当前processor线程中队列中元素的个数【{}】。", index, processorList.get(index).getQueueSize());
    return processorHandlerByLinkedBlockingQueueList.get(index);
  }

}
