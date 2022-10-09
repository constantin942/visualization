package com.mingshi.skyflying.tzx;

import com.lmax.disruptor.BlockingWaitStrategy;
import com.lmax.disruptor.RingBuffer;
import com.lmax.disruptor.dsl.Disruptor;
import com.lmax.disruptor.dsl.ProducerType;

import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/17
 */
public class TestDisruptor {
    public static void main(String[] args) {
        StringEventFactory factory = new StringEventFactory();
        int bufferSize = 1024;
        Disruptor<StringEvent> disruptor = new Disruptor<>(factory, bufferSize, Executors.newCachedThreadPool());
        disruptor.handleEventsWith(new StringEventHandler());
        disruptor.start();
        RingBuffer<StringEvent> ringBuffer = disruptor.getRingBuffer();
        for(int i = 0; i < 1024; i++) {
            long next = ringBuffer.next();
            StringEvent stringEvent = ringBuffer.get(next);
            stringEvent.setStringEvent(String.valueOf(i));
            ringBuffer.publish(next);
        }
    }
}
