package com.mingshi.skyflying.test.init;

import com.mingshi.skyflying.test.TestProcesserAndIoThreaderList;
import com.mingshi.skyflying.test.thread.TestIoThreaderThread;
import com.mingshi.skyflying.test.thread.TestProcessorThread;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * <B>主类名称: TestProcessorQueue</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/20 16:14
 *
 * @Version 1.0
 **/
@Component
public class InitTestProcessorAndIoThreaderQueue implements ApplicationRunner {

  private volatile static Boolean initProcessorSuccess = false;

  private volatile static Boolean initIoThreaderSuccess = false;

  public static boolean getInitProcessorSuccess() {
    return initProcessorSuccess;
  }

  public static boolean getInitIoThreaderSuccess() {
    return initIoThreaderSuccess;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    // 创建4个Processor线程，每个Processor线程对应一个自己的内存队列；
    createProcessorThread();
    initProcessorSuccess = true;
    createIoThreaderThread();
    initIoThreaderSuccess = true;
  }

  private static void createProcessorThread() {
    for (int i = 0; i < 1; i++) {
      TestProcessorThread testProcessorThread = new TestProcessorThread();
      testProcessorThread.setName("testProcessorThread_" + i);
      testProcessorThread.start();
      TestProcesserAndIoThreaderList.getProcessorThreadList().add(testProcessorThread);
    }
  }

  private static void createIoThreaderThread() {
    for (int i = 0; i < 1; i++) {
      TestIoThreaderThread testIoThreaderThread = new TestIoThreaderThread();
      testIoThreaderThread.setName("testIoThreaderThread_" + i);
      testIoThreaderThread.start();
      TestProcesserAndIoThreaderList.getIoThreaderThreadList().add(testIoThreaderThread);
    }
  }
}
