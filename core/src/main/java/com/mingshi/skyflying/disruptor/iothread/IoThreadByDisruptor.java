package com.mingshi.skyflying.disruptor.iothread;


import com.fasterxml.jackson.databind.node.ObjectNode;
import com.lmax.disruptor.BlockingWaitStrategy;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;
import com.mingshi.skyflying.elasticsearch.utils.EsMsSegmentDetailUtil;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * <B>类名称：IoThreadByDisruptor</B>
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
public class IoThreadByDisruptor implements ApplicationRunner {
  // 是否开启reactor模式的开关；2022-06-01 09:28:28
  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;

  // processor线程的数量；2022-06-01 09:28:57
  @Value("${reactor.processor.thread.count}")
  private Integer reactorProcessorThreadCount;

  @Value("${reactor.iothread.disruptor}")
  private boolean reactorIoThreadByDisruptor;

  @Resource
  MingshiServerUtil mingshiServerUtil;

  @Resource
  EsMsSegmentDetailUtil esMsSegmentDetailUtil;

  private AtomicInteger atomicInteger = new AtomicInteger(0);

  // 在开启reactor模式的情况下，创建ioThread线程的数量；2022-06-01 09:28:57
  @Value("${reactor.iothread.thread.count}")
  private Integer reactorIoThreadThreadCount;

  // private Integer queueSize = 4;
  private Integer queueSize = 4096;
  private RingBuffer<IoThreadObjectNode> ringBuffer;
  private String applicationName;

  private volatile Boolean createIoThreadsFinishedFlag = false;

  /**
   * <B>方法名称：disruptorInitDone</B>
   * <B>概要说明：Disruptor初始化是否已完成</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月24日 11:07:30
   * @Param []
   **/
  public void disruptorInitDone() {
    if (false == createIoThreadsFinishedFlag) {
      while (true) {
        if (true == createIoThreadsFinishedFlag) {
          break;
        } else {
          try {
            TimeUnit.MILLISECONDS.sleep(100);
          } catch (InterruptedException e) {
            // ignore
          }
        }
      }
    }
  }

  /**
   * <B>方法名称：init</B>
   * <B>概要说明：初始化Disruptor实例</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月24日 11:07:53
   * @Param [applicationName]
   **/
  private void init(String applicationName) {
    this.applicationName = applicationName;
    this.ringBuffer = messageModelRingBuffer(queueSize, 1 == reactorProcessorThreadCount ? true : false);
    createIoThreadsFinishedFlag = true;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    if (true == reactorProcessorEnable && true == reactorIoThreadByDisruptor) {
      log.info("开始执行 # IoThreadByDisruptor.run() # 项目启动，开始构造Disruptor实例。");
      init("ioThread_");
      log.info("执行完毕 # IoThreadByDisruptor.run() # 项目启动，构造Disruptor实例完毕。");
    }
  }

  public RingBuffer<IoThreadObjectNode> messageModelRingBuffer(Integer queueSize, Boolean singleProducer) {
    // RingBuffer是一个数组，数组中的每个位置都存放一个对象/实例。
    // 下面这行代码，就是要指定创建对象/实例的工厂。这行代码会在RingBuffer创建完毕之后，给每个位置存放对象/实例时用到。2022-07-17 10:45:23
    ConsumerIoThreadFactory factory = new ConsumerIoThreadFactory();

    // 创建线程的工厂。2022-07-17 10:45:42
    ThreadFactory producerFactory = Executors.defaultThreadFactory();
    // 指定ringbuffer大小，必须为2的N次方（能将求模运算转为位运算提高效率），否则将影响效率
    Disruptor<IoThreadObjectNode> disruptor = null;

    // 根据Processor线程的数量，来决定是单生产者还是多生产者；2022-07-27 17:48:01
    if(null != reactorProcessorThreadCount && 1 == reactorProcessorThreadCount){
      // 在批处理的情况下，使用单生产者；2021-12-23 08:30:33
      disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.SINGLE, new BlockingWaitStrategy());
    }else{
      disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.MULTI, new BlockingWaitStrategy());
    }

    if(null != reactorIoThreadThreadCount && 1 == reactorIoThreadThreadCount){
      log.info("# IoThreadByDisruptor.messageModelRingBuffer() # 根据配置文件设置的IoThread线程的数量 = 【{}】，由此创建单消费者线程。", reactorIoThreadThreadCount);
      // 使用单消费者模式；
      IoThreadConsumerByEventHandler ioThreadConsumerByEventHandler = new IoThreadConsumerByEventHandler(10, mingshiServerUtil, esMsSegmentDetailUtil, ringBuffer);
      // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
      disruptor.handleEventsWith(ioThreadConsumerByEventHandler);
    }else{
      log.info("# IoThreadByDisruptor.messageModelRingBuffer() # 根据配置文件设置的IoThread线程的数量 = 【{}】，由此创建多消费者线程。", reactorIoThreadThreadCount);
      for (Integer integer = 0; integer < reactorIoThreadThreadCount; integer++) {
        log.info("# IoThreadByDisruptor.messageModelRingBuffer() # 根据配置文件设置的IoThread线程的数量 = 【{}】，由此创建多消费者线程。开始创建第【{}】线程。", reactorIoThreadThreadCount, (integer + 1));
        // 使用多消费者模式；
        IoThreadConsumerByWorkHandler ioThreadConsumerByWorkHandler = new IoThreadConsumerByWorkHandler(10, mingshiServerUtil, esMsSegmentDetailUtil, ringBuffer);
        // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
        disruptor.handleEventsWithWorkerPool(ioThreadConsumerByWorkHandler);
      }
    }

    // if (true == singleProducer) {
    //   log.info("# IoThreadByDisruptor.messageModelRingBuffer() # 根据配置文件设置的IoThread线程的数量 = 【{}】，由此创建单消费者线程。", reactorIoThreadThreadCount);
    //   // 在批处理的情况下，使用单生产者；2021-12-23 08:30:33
    //   disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.SINGLE, new BlockingWaitStrategy());
    //   // 使用单消费者模式；
    //   IoThreadConsumerByEventHandler ioThreadConsumerByEventHandler = new IoThreadConsumerByEventHandler(10, mingshiServerUtil, esMsSegmentDetailUtil, ringBuffer);
    //   // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
    //   disruptor.handleEventsWith(ioThreadConsumerByEventHandler);
    // } else {
    //   log.info("# IoThreadByDisruptor.messageModelRingBuffer() # 根据配置文件设置的IoThread线程的数量 = 【{}】，由此创建多消费者线程。", reactorIoThreadThreadCount);
    //   // 在非批处理的情况下，使用多生产者；2021-12-23 08:30:54
    //   disruptor = new Disruptor<>(factory, queueSize, producerFactory, ProducerType.MULTI, new BlockingWaitStrategy());
    //   for (Integer integer = 0; integer < reactorIoThreadThreadCount; integer++) {
    //     log.info("# IoThreadByDisruptor.messageModelRingBuffer() # 根据配置文件设置的IoThread线程的数量 = 【{}】，由此创建多消费者线程。开始创建第【{}】线程。", reactorIoThreadThreadCount, (integer + 1));
    //     // 使用多消费者模式；
    //     IoThreadConsumerByWorkHandler ioThreadConsumerByWorkHandler = new IoThreadConsumerByWorkHandler(10, mingshiServerUtil, esMsSegmentDetailUtil, ringBuffer);
    //     // 将创建好消费对象/实例的handler与RingBuffer关联起来；2022-07-17 18:54:56
    //     disruptor.handleEventsWithWorkerPool(ioThreadConsumerByWorkHandler);
    //   }
    // }

    // 启动disruptor线程
    disruptor.start();

    // 获取ringbuffer环，用于接取生产者生产的事件
    RingBuffer<IoThreadObjectNode> ringBuffer = disruptor.getRingBuffer();

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
  public boolean offer(ObjectNode data) {
    // 获取待存放元素的数组下标；2022-07-17 10:34:41
    long sequence = ringBuffer.next();
    try {
      // 根据下标，从数组中获取该下标位置的对象/实例；2022-07-17 10:36:01
      IoThreadObjectNode ioThreadObjectNode = ringBuffer.get(sequence);
      // 给获取到的对象/实例赋值；2022-07-17 10:36:34
      ioThreadObjectNode.setData(data);
      return true;
    } catch (Exception e) {
      log.error("当前线程【{}】往disruptor中存放数据时，出现了异常。", Thread.currentThread().getName(), e);
      return false;
    } finally {
      // 将该下标位置的对象发布出去。也就是告诉消费者，队列里有数据了，可以来消费了。2022-07-17 10:37:00
      ringBuffer.publish(sequence);

      // 间隔输出日志；2022-07-22 21:33:52
      // Integer incrementAndGet = atomicInteger.incrementAndGet();
      // int i = incrementAndGet & (1024 * 2 - 1);
      // if (0 == i) {
      //   log.info("当前线程【{}】将处理后的流量放入到IoThreadObjectNodeRingBuffer中，当前该队列可用容量【{}】，总容量【{}】。", Thread.currentThread().getName(), ringBuffer.remainingCapacity(), queueSize);
      // }
    }
  }

  // 获取队列中有多少个元素；2021-10-20 15:22:55
  public long getQueueSize() {
    return queueSize - ringBuffer.remainingCapacity();
  }

}
