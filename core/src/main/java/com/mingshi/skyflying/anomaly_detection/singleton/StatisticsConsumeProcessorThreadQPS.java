package com.mingshi.skyflying.anomaly_detection.singleton;

import com.mingshi.skyflying.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * <B>方法名称：StatisticsConsumeProcessorThreadQPS</B>
 * <B>概要说明：统计消费线程的QPS</B>
 *
 * @Author zm
 * @Date 2022年06月24日 10:06:26
 * @Param
 * @return
 **/
@Slf4j
public class StatisticsConsumeProcessorThreadQPS {
  private static volatile Map<String/* 线程名称 */, Map<String/* 时间 */, AtomicInteger/* 在当前时间内的处理消息的数量 */>> statisticsConsumeProcessorThreadQPSMap = null;
  // private static volatile Map<String/* 时间 */, AtomicInteger/* 在当前时间内的处理消息的数量 */> timeCountMap = null;

  private static volatile AtomicInteger atomicInteger = new AtomicInteger(0);

  // public static Map<String/* 时间 */, AtomicInteger/* 在当前时间内的处理消息的数量 */> getTimeCountMap() {
  //   if (null == timeCountMap) {
  //     synchronized (StatisticsConsumeProcessorThreadQPS.class) {
  //       if (null == timeCountMap) {
  //         timeCountMap = new ConcurrentHashMap<>();
  //       }
  //     }
  //   }
  //   return timeCountMap;
  // }

  public static Map<String/* 线程名称 */, Map<String/* 时间 */, AtomicInteger/* 在当前时间内的处理消息的数量 */>> getStatisticsConsumeProcessorThreadQPSMap() {
    if (null == statisticsConsumeProcessorThreadQPSMap) {
      synchronized (StatisticsConsumeProcessorThreadQPS.class) {
        if (null == statisticsConsumeProcessorThreadQPSMap) {
          statisticsConsumeProcessorThreadQPSMap = new ConcurrentHashMap<>();
        }
      }
    }
    return statisticsConsumeProcessorThreadQPSMap;
  }

  // 统计每个processor线程在每秒中的QPS；2022-06-24 10:28:45
  public static void accumulateTimes(String threadName, String time) {
    try {
      int incrementAndGet = atomicInteger.incrementAndGet();
      if (0 == incrementAndGet % (10 * 10000)) {
        // 每1000次后输出当前的QPS；
        // log.info(" # StatisticsConsumeProcessorThreadQPS.accumulateTimes() # 各个线程的QPS =【{}】。", JsonUtil.obj2String(timeCountMap));
        log.info(" # StatisticsConsumeProcessorThreadQPS.accumulateTimes() # 各个线程的QPS = 【{}】。", JsonUtil.obj2String(statisticsConsumeProcessorThreadQPSMap));
        atomicInteger.set(0);
      }
      // timeCount(time);
      threadNameTimeCount(threadName, time);

    } catch (Exception e) {
      log.error(" # StatisticsConsumeProcessorThreadQPS.accumulateTimes() # 统计每个processor线程在每秒中的QPS时，出现了异常。", e);
    }
  }

  private static void threadNameTimeCount(String threadName, String time) {
    getStatisticsConsumeProcessorThreadQPSMap();
    Map<String, AtomicInteger> stringAtomicIntegerMap = statisticsConsumeProcessorThreadQPSMap.get(threadName);
    if (null == stringAtomicIntegerMap) {
      stringAtomicIntegerMap = new HashMap<>();
      statisticsConsumeProcessorThreadQPSMap.put(threadName, stringAtomicIntegerMap);
    }
    AtomicInteger atomicInteger = stringAtomicIntegerMap.get(time);
    if (null == atomicInteger) {
      atomicInteger = new AtomicInteger(0);
    }
    atomicInteger.incrementAndGet();
    stringAtomicIntegerMap.put(time, atomicInteger);
  }

  // private static void timeCount(String time) {
  //   getTimeCountMap();
  //   AtomicInteger accumulate = timeCountMap.get(time);
  //   if (null == accumulate) {
  //     accumulate = new AtomicInteger(0);
  //     timeCountMap.put(time, accumulate);
  //   }
  //   accumulate.incrementAndGet();
  // }

}
