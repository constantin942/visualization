/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mingshi.skyflying.kafka.consumer;

import com.google.protobuf.InvalidProtocolBufferException;
import kafka.utils.ShutdownableThread;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.BytesDeserializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;

import java.util.Collections;
import java.util.Properties;

public class Consumer extends ShutdownableThread {
    private final KafkaConsumer<String, Bytes> consumer;
    private final String topic;

    public Consumer(String topic) {
        super("KafkaConsumerExample", false);
        Properties props = new Properties();
        props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "10.0.107.49:9092");
        props.put(ConsumerConfig.GROUP_ID_CONFIG, "skyflying-consumer-group");
        props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "true");
        props.put(ConsumerConfig.AUTO_COMMIT_INTERVAL_MS_CONFIG, "1000");
        props.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, "30000");

      String name = new StringDeserializer().getClass().getName();
      props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, name);
      String value = new BytesDeserializer().getClass().getName();
      props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, value);

        consumer = new KafkaConsumer<>(props);
        this.topic = topic;
    }

  @Override
  public void run() {
    while(true){
      doWork();
    }
  }

  @Override
    public void doWork() {
        consumer.subscribe(Collections.singletonList(this.topic));
        ConsumerRecords<String, Bytes> records = consumer.poll(1000);
        for (ConsumerRecord<String, Bytes> record : records) {
            System.out.println("Received message: (" + record.key() + ", " + record.value() + ") at offset " + record.offset());
          Bytes bytes = (Bytes) record.value();
          byte[] bytesArray = bytes.get();
          try {
            SegmentObject segmentObject = SegmentObject.parseFrom(bytesArray);
            System.out.println("");
          } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
          }

        }
    }

    @Override
    public String name() {
        return null;
    }

    @Override
    public boolean isInterruptible() {
        return false;
    }
}
