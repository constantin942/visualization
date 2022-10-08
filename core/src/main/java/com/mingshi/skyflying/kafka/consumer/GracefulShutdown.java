package com.mingshi.skyflying.kafka.consumer;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import javax.annotation.PreDestroy;

/**
 * <B>主类名称: GracefulShutdown</B>
 * <B>概要说明：优雅关机全局类</B>
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
