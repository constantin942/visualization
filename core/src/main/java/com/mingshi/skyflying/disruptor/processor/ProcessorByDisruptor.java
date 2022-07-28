package com.mingshi.skyflying.disruptor.processor;


import com.lmax.disruptor.BlockingWaitStrategy;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;
import com.mingshi.skyflying.service.SegmentConsumerService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * <B>类名称：ProcessorByDisruptor</B>
 * <B>概要说明：项目启动，构建一个Disruptor实例</B>
 *
 * @Author zm
 * @Date 2022年07月22日 20:07:28
 * @Param
 * @return
 **/
@Slf4j
@Component
@PropertySource("classpath:application-${spring.profiles.active}.yml")
public class ProcessorByDisruptor implements ApplicationRunner {
  // 是否开启reactor模式的开关；2022-06-01 09:28:28
  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorByDisruptor;

  private AtomicInteger atomicInteger = new AtomicInteger(0);

  @Resource
  private SegmentConsumerService segmentConsumerService;
  // 在开启reactor模式的情况下，创建processor线程的数量；2022-06-01 09:28:57
  @Value("${reactor.processor.thread.count}")
  private Integer reactorProcessorThreadCount;

  // private Integer queueSize = 4;
  private Integer queueSize = 1024;
  private RingBuffer<SegmentByByte> acceptorRingBuffer;

  private volatile Boolean createProcessorsFinishedFlag = false;

  public Boolean getCreateProcessorsFinishedFlag() {
    return createProcessorsFinishedFlag;
  }

  private void init(String applicationName) {
    this.acceptorRingBuffer = messageModelRingBuffer(queueSize);
    createProcessorsFinishedFlag = true;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    if (true == reactorProcessorEnable && true == reactorProcessorByDisruptor) {
      log.info("开始执行 # ProcessorByDisruptor.run() # 项目启动，开始构造Disruptor实例。");
      init("acceptor_");
      log.info("执行完毕 # ProcessorByDisruptor.run() # 项目启动，构造Disruptor实例完毕。");
    }
  }

  public RingBuffer<SegmentByByte> messageModelRingBuffer(Integer queueSize) {
    // RingBuffer是一个数组，数组中的每个位置都存放一个对象/实例。
    // 下面这行代码，就是要指定创建对象/实例的工厂。这行代码会在RingBuffer创建完毕之后，给每个位置存放对象/实例时用到。2022-07-17 10:45:23
    ConsumerProcessorFactory factory = new ConsumerProcessorFactory();

    // 创建线程的工厂。2022-07-17 10:45:42
    ThreadFactory producerFactory = Executors.defaultThreadFactory();
    // 指定ringbuffer大小，必须为2的N次方（能将求模运算转为位运算提高效率），否则将影响效率
    Disruptor<SegmentByByte> disruptor = null;

    // 在批处理的情况下，使用单生产者；2021-12-23 08:30:33
    // disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.SINGLE, new BlockingWaitStrategy());

    // 使用多生产者；2022-07-28 14:42:07
    disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.MULTI, new BlockingWaitStrategy());

    if (null != reactorProcessorThreadCount && 1 == reactorProcessorThreadCount) {
      log.info("# ProcessorByDisruptor.messageModelRingBuffer() # 根据配置文件设置的Processor线程的数量 = 【{}】，由此创建单消费者线程。", reactorProcessorThreadCount);
      // 使用单消费者模式；
      ProcessorConsumerByEventHandler processorConsumerByEventHandler = new ProcessorConsumerByEventHandler(segmentConsumerService);
      // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
      disruptor.handleEventsWith(processorConsumerByEventHandler);
    } else {
      // 使用多消费者模式；
      for (Integer integer = 0; integer < reactorProcessorThreadCount; integer++) {
        log.info("# ProcessorByDisruptor.messageModelRingBuffer() # 根据配置文件设置的Processor线程的数量 = 【{}】，由此创建多消费者线程。开始创建第【{}】消费者线程。", reactorProcessorThreadCount, (integer + 1));
        ProcessorConsumerByWrokHandler processorConsumerByWrokHandler = new ProcessorConsumerByWrokHandler(segmentConsumerService);
        // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
        disruptor.handleEventsWithWorkerPool(processorConsumerByWrokHandler);
      }
    }


    // if (true == enableBatch) {
    //   log.info("# ProcessorByDisruptor.messageModelRingBuffer() # 根据配置文件设置的Processor线程的数量 = 【{}】，由此创建单消费者线程。", reactorProcessorThreadCount);
    //   // 在批处理的情况下，使用单生产者；2021-12-23 08:30:33
    //   disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.SINGLE, new BlockingWaitStrategy());
    //   // 使用单消费者模式；
    //   ProcessorConsumerByEventHandler processorConsumerByEventHandler = new ProcessorConsumerByEventHandler(segmentConsumerService);
    //   // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
    //   disruptor.handleEventsWith(processorConsumerByEventHandler);
    // } else {
    //   log.info("# ProcessorByDisruptor.messageModelRingBuffer() # 根据配置文件设置的Processor线程的数量 = 【{}】，由此创建多消费者线程。", reactorProcessorThreadCount);
    //   // 在非批处理的情况下，使用多生产者；2021-12-23 08:30:54
    //   disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.MULTI, new BlockingWaitStrategy());
    //   for (Integer integer = 0; integer < reactorProcessorThreadCount; integer++) {
    //     // 使用多消费者模式；
    //     ProcessorConsumerByWrokHandler processorConsumerByWrokHandler = new ProcessorConsumerByWrokHandler(segmentConsumerService);
    //     // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
    //     disruptor.handleEventsWithWorkerPool(processorConsumerByWrokHandler);
    //   }
    // }

    // 启动disruptor线程
    disruptor.start();

    // 获取ringbuffer环，用于接取生产者生产的事件
    RingBuffer<SegmentByByte> ringBuffer = disruptor.getRingBuffer();

    return ringBuffer;
  }

  /**
   * <B>方法名称：offer</B>
   * <B>概要说明：将数据放入到RingBuffer中</B>
   *
   * @return boolean
   * @Author zm
   * @Date 2022年07月22日 20:07:29
   * @Param [data]
   **/
  public boolean offer(byte[] data) {
    // 获取待存放元素的数组下标；2022-07-17 10:34:41
    long sequence = acceptorRingBuffer.next();
    try {
      // 根据下标，从数组中获取该下标位置的对象/实例；2022-07-17 10:36:01
      SegmentByByte segmentByByte = acceptorRingBuffer.get(sequence);
      // 给获取到的对象/实例赋值；2022-07-17 10:36:34
      segmentByByte.setData(data);
      return true;
    } catch (Exception e) {
      log.error("当前线程【{}】往disruptor中存放数据时，出现了异常。", Thread.currentThread().getName());
      return false;
    } finally {
      // 将该下标位置的对象发布出去。也就是告诉消费者，队列里有数据了，可以来消费了。2022-07-17 10:37:00
      acceptorRingBuffer.publish(sequence);

      // 间隔输出日志；2022-07-22 21:33:52
      // Integer incrementAndGet = atomicInteger.incrementAndGet();
      // int i = incrementAndGet & (1024 * 2 - 1);
      // if (0 == i) {
      //   log.info("当前线程【{}】将没有处理前的流量放入到FlowsMatchRingBuffer中，当前队列中元素个数【{}】，总容量【{}】。",
      //     Thread.currentThread().getName(), getQueueSize(), queueSize);
      // }
    }
  }

  // 获取队列中元素的个数；2021-10-20 15:22:55
  public long getQueueSize() {
    return queueSize - acceptorRingBuffer.remainingCapacity();
  }

}
