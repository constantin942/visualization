package com.mingshi.skyflying.anomaly_detection.controller;

import com.mingshi.skyflying.anomaly_detection.dao.CoarseSegmentDetailOnTimeMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.VisitCountOnTimeInterval;
import com.mingshi.skyflying.anomaly_detection.service.UserPortraitByTimeTask;
import com.mingshi.skyflying.anomaly_detection.service.impl.SegmentDetailServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/26
 */
@RestController
public class TestControllerA {
    @Autowired
    UserPortraitByTimeTask timeTask;
    @GetMapping("test")
    public void test() {
        timeTask.createUserPortraitByTime(15);
    }

}
