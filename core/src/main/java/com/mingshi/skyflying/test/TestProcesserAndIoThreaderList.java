package com.mingshi.skyflying.test;

import com.mingshi.skyflying.test.init.InitTestProcessorAndIoThreaderQueue;
import com.mingshi.skyflying.test.thread.TestIoThreaderThread;
import com.mingshi.skyflying.test.thread.TestProcessorThread;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: TestProcesserAndIoThreaderList</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/20 16:26
 *
 * @Version 1.0
 **/
@Slf4j
public class TestProcesserAndIoThreaderList {
  private volatile static List<TestProcessorThread> processorThreadList = new ArrayList<>();

  private volatile static List<TestIoThreaderThread> ioThreaderThreadList = new ArrayList<>();

  public static List<TestProcessorThread> getProcessorThreadList() {
    return processorThreadList;
  }

  public static List<TestIoThreaderThread> getIoThreaderThreadList() {
    return ioThreaderThreadList;
  }

  /**
   * 根据partition确定Processor线程；
   * @param partition
   * @return
   */
  public static TestProcessorThread getTestProcessorThread(Integer partition) {
    while (!InitTestProcessorAndIoThreaderQueue.getInitProcessorSuccess()) {
      // 当Processor线程还没有创建成功时，等待500毫秒；
      try {
        TimeUnit.MILLISECONDS.sleep(500);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
    Integer index = partition & (processorThreadList.size() - 1);
    return processorThreadList.get(index);
  }

  /**
   * 根据partition确定IoThreader线程；
   * @param partition
   * @return
   */
  public static TestIoThreaderThread getTestIoThreaderThread(Integer partition) {
    while (!InitTestProcessorAndIoThreaderQueue.getInitIoThreaderSuccess()) {
      // 当IoThreader线程还没有创建成功时，等待500毫秒；
      try {
        TimeUnit.MILLISECONDS.sleep(500);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
    Integer index = partition & (ioThreaderThreadList.size() - 1);
    return ioThreaderThreadList.get(index);
  }

}
