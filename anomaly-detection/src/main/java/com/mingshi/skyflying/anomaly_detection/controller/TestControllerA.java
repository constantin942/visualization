package com.mingshi.skyflying.anomaly_detection.controller;

import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/26
 */
@RestController
public class TestControllerA {

    @Resource
    UserPortraitByTimeTask timeTask;


    @GetMapping("test")
    public void test() {
        List<UserPortraitByTimeDo> list = new ArrayList<>();
        list.add(UserPortraitByTimeDo.builder()
                .username("testTzx")
                .morningRate(0.2).afternoonRate(0.3).nightRate(0.5).build());
        timeTask.cachePortraitByTime(list);
    }

}
