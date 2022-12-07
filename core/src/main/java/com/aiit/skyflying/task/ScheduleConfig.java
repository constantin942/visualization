package com.aiit.skyflying.task;

import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.SchedulingConfigurer;
import org.springframework.scheduling.config.ScheduledTaskRegistrar;

import java.util.concurrent.ScheduledThreadPoolExecutor;

/**
 * <B>方法名称：ScheduleConfig</B>
 * <B>概要说明：定时器多线程配置</B>
 *
 * @Author zm
 * @Date 2022-10-13 10:50:40
 * @Param
 * @return
 **/
@Configuration
public class ScheduleConfig implements SchedulingConfigurer {

    @Override
    public void configureTasks(ScheduledTaskRegistrar taskRegistrar) {
        taskRegistrar.setScheduler(new ScheduledThreadPoolExecutor(Runtime.getRuntime().availableProcessors(), r -> new Thread(r,"my-schedule")));
    }
}
