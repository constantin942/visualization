package com.mingshi.skyflying.test.thread;

import com.mingshi.skyflying.test.TestProcesserAndIoThreaderList;
import com.mingshi.skyflying.test.domain.TestRecordAckDo;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;

import java.util.concurrent.LinkedBlockingQueue;

/**
 * <B>主类名称: TestProcessorThread</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/20 16:13
 *
 * @Version 1.0
 **/
@Slf4j
public class TestProcessorThread extends Thread {
  /**
   * 创建一个阻塞队列，大小是100；2022-08-20 16:22:39
   */
  private LinkedBlockingQueue<TestRecordAckDo> linkedBlockingQueue = new LinkedBlockingQueue<>(500);

  /**
   * 往队列里存放数据，当队列满时，则阻塞住；2022-08-20 16:22:11
   * @param testRecordAckDo
   */
  public void offer(TestRecordAckDo testRecordAckDo) {
    try {
      linkedBlockingQueue.put(testRecordAckDo);
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  /**
   * 不断的从队列里获取数据，并消费；
   */
  @Override
  public void run() {
    while (true) {
      try {
        TestRecordAckDo testRecordAckDo = linkedBlockingQueue.take();
        ConsumerRecord<String, Bytes> record = testRecordAckDo.getRecord();
        Integer partition = record.partition();
        // 根据partition，将消息路由到指定的IoThreader线程那里；2022-08-20 16:44:17
        TestIoThreaderThread testIoThreaderThread = TestProcesserAndIoThreaderList.getTestIoThreaderThread(partition);
        testIoThreaderThread.offer(testRecordAckDo);
      } catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
  }
}
