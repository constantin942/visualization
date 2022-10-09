package com.mingshi.skyflying.tzx;

import com.lmax.disruptor.EventFactory;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/17
 */
public class StringEventFactory implements EventFactory<StringEvent> {

    @Override
    public StringEvent newInstance() {
        return new StringEvent();
    }
}
