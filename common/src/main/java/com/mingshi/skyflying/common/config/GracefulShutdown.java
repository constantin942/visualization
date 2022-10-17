package com.mingshi.skyflying.common.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import javax.annotation.PreDestroy;

/**
 * <B>主类名称: GracefulShutdown</B>
 * <B>概要说明：优雅关机全局类</B>
 *
 * 优雅关机执行的步骤：
 *    1. 当系统正常关闭时，全局变量RUNNING由true变为false；
 *    2. 消费者实例aiitKafkaConsumer感知到全局变量RUNNING的变化后，不再向Kafka服务端拉取消息，等待所有的IoThread线程退出后，再提交同步提交offset；
 *    3. 当Processor线程感知到全局变量RUNNING的变化后，退出死循环，等消费完自己的阻塞队列里的消息后，当前线程生命周期就结束了；
 *    4. 当IoThread线程感知到全局变量RUNNING的变化后，退出死循环，等消费完自己的阻塞队列里的消息后，当前线程生命周期就结束了。
 *    即：等待Processor线程对应的阻塞队列和IoThread线程对应的阻塞队列中都没有消息后，消费者实例aiitKafkaConsumer同步提交offset，此时消息就不会丢失了。
 *
 * Author zm
 * Date 2022/10/8 11:10
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class GracefulShutdown {
    private volatile static Boolean RUNNING = true;

    @PreDestroy
    public static void setRunning() {
        log.info("# GracefulShutdown.setRunning() # 项目关闭，将全局开关关闭。");
        RUNNING = false;
    }

    public static Boolean getRUNNING(){
        return RUNNING;
    }
}
