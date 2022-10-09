package com.mingshi.skyflying.tzx;

import com.lmax.disruptor.EventHandler;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/17
 */
public class StringEventHandler implements EventHandler<StringEvent> {

    @Override
    public void onEvent(StringEvent stringEvent, long l, boolean b) throws Exception {
        System.out.println(Thread.currentThread().getName() + ":" + stringEvent.getStringEvent());
    }
}
