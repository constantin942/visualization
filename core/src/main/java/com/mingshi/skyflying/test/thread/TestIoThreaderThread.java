package com.mingshi.skyflying.test.thread;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.test.domain.TestRecordAckDo;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.kafka.support.Acknowledgment;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * <B>主类名称: TestIoThreaderThread</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/20 16:13
 *
 * @Version 1.0
 **/
@Slf4j
public class TestIoThreaderThread extends Thread {

  /**
   * 创建一个阻塞队列，大小是100；2022-08-20 16:22:39
   */
  private LinkedBlockingQueue<TestRecordAckDo> linkedBlockingQueue = new LinkedBlockingQueue<>(500);
  private Map<String/* topic */, Map<Integer/* partition */, Map<Long/* offset */, Acknowledgment>>> map = new ConcurrentHashMap<>();
  private Instant now = Instant.now();
  private Long INTERVAL = 20L;

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
        TestRecordAckDo testRecordAckDo = linkedBlockingQueue.poll();
        if (null != testRecordAckDo) {
          ConsumerRecord<String, Bytes> record = testRecordAckDo.getRecord();
          Acknowledgment ack = testRecordAckDo.getAck();

          String topic = record.topic();
          Integer partition = record.partition();
          Long newOffset = record.offset();
          Map<Integer/* partition */, Map<Long/* offset */, Acknowledgment>> integerLongMap = map.get(topic);
          if (null == integerLongMap) {
            integerLongMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
            map.put(topic, integerLongMap);
          }
          Map<Long/* offset */, Acknowledgment> offsetAckMap = integerLongMap.get(partition);
          if (null == offsetAckMap) {
            offsetAckMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
            offsetAckMap.put(newOffset, ack);
            integerLongMap.put(partition, offsetAckMap);
          } else {
            offsetAckMap.put(newOffset, ack);
            Iterator<Long> iterator = offsetAckMap.keySet().iterator();
            while (iterator.hasNext()) {
              Long oldOffset = iterator.next();
              if (oldOffset < newOffset) {
                offsetAckMap.clear();
                // 只保存这个分区中已消费过的最大消息的offset；2022-08-20 17:03:54
                offsetAckMap.put(newOffset, ack);
              } else if(oldOffset > newOffset){
                log.error("要开始更新offset对应的Acknowledgment实例了。老的offset = {} 比 新的offset = {} 大，这是不合理的。出现了问题，需要定位排查下。", oldOffset, newOffset);
              }
            }
          }
        } else {
          Long interval = DateTimeUtil.getSecond(now);
          if (INTERVAL < interval) {
            now = Instant.now();
            Boolean flag = false;
            // 每隔一定的时间，将数据持久化，然后把这一段时间内消费的消息提交offset；2022-08-20 16:48:19
            Iterator<String> iterator = map.keySet().iterator();

            while (iterator.hasNext()) {
              String topic = iterator.next();
              Map<Integer, Map<Long/* offset */, Acknowledgment>> integerListMap = map.get(topic);
              Iterator<Integer> iterator1 = integerListMap.keySet().iterator();
              while (iterator1.hasNext()) {
                Integer partition = iterator1.next();
                Map<Long/* offset */, Acknowledgment> offsetMap = integerListMap.get(partition);
                Iterator<Long> iterator2 = offsetMap.keySet().iterator();
                while (iterator2.hasNext()) {
                  Long next = iterator2.next();
                  log.info("当前线程 = {}，要提交offset = {} 到Kafka Broker端了。", Thread.currentThread().getName(), next);
                  Acknowledgment acknowledgment = offsetMap.get(next);
                  // 提交这个分区中目前已消费过的最大消息的offset；
                  acknowledgment.acknowledge();
                  flag = true;
                }
              }
            }
            if (true == flag) {
              map.clear();
            }
          }
        }
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }
}
