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
  /**
   * 是否开启reactor模式的开关；2022-06-01 09:28:28
   */
  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorByDisruptor;

  @Resource
  private SegmentConsumerService segmentConsumerService;
  /**
   * 在开启reactor模式的情况下，创建processor线程的数量；2022-06-01 09:28:57
   */
  @Value("${reactor.processor.thread.count}")
  private Integer reactorProcessorThreadCount;

  private Integer queueSize = 8192;
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
    // 这里使用单生产者还是多生产者，取决于kafka的消费者线程的数量。2022-07-28 16:35:59
    // 通过集成原生Kafka的消费端代码，手动创建了一个消费线程，从Kafka服务端拉取消息。所以这里可以指定使用单生产者模式。2022-07-28 17:44:13
    disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.SINGLE, new BlockingWaitStrategy());

    // 使用多生产者；2022-07-28 14:42:07
    // 消费者使用yield等待策略，在实测中，并不能提高QPS。同时会导致CPU一直飙高到100%，就算没有消息要消费，CPU也会一直飙升到100%。不建议使用.2022-07-28 16:31:52
    // disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.MULTI, new YieldingWaitStrategy());
    // 消费者使用blocking阻塞策略，在实测中，比较温和，在有消息要处理的情况下，CPU一般占据70%。推荐使用。2022-07-28 16:32:54
    // disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.MULTI, new BlockingWaitStrategy());

    if (null != reactorProcessorThreadCount && 1 == reactorProcessorThreadCount) {
      log.info("# ProcessorByDisruptor.messageModelRingBuffer() # 根据配置文件设置的Processor线程的数量 = 【{}】，由此创建单消费者线程。", reactorProcessorThreadCount);
      // 使用单消费者模式。这个消费者消费队列中所有的消息。类似于RocketMQ中广播消息模式。
      // 注意：如果创建多个ProcessorConsumerByEventHandler，那么每条消息都会被所有的消费者消费。2022-07-28 16:35:10
      ProcessorConsumerByEventHandler processorConsumerByEventHandler = new ProcessorConsumerByEventHandler(segmentConsumerService);
      // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
      disruptor.handleEventsWith(processorConsumerByEventHandler);
    } else {
      // 使用多消费者模式.每个消费者只消费队列中一部分消息。类似于RocketMQ中集群消费模式。
      ProcessorConsumerByWrokHandler[] processorConsumerByWrokHandlerArray = new ProcessorConsumerByWrokHandler[reactorProcessorThreadCount];
      for (int i = 0; i < reactorProcessorThreadCount; i++) {
        log.info("# ProcessorByDisruptor.messageModelRingBuffer() # 根据配置文件设置的Processor线程的数量 = 【{}】，由此创建多消费者线程。开始创建第【{}】消费者线程。", reactorProcessorThreadCount, (i + 1));
        processorConsumerByWrokHandlerArray[i] = new ProcessorConsumerByWrokHandler(segmentConsumerService);
      }
      disruptor.handleEventsWithWorkerPool(processorConsumerByWrokHandlerArray);
    }

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

  /**
   * 获取队列中元素的个数；2021-10-20 15:22:55
   * @return
   */
  public long getQueueSize() {
    return queueSize - acceptorRingBuffer.remainingCapacity();
  }

}
