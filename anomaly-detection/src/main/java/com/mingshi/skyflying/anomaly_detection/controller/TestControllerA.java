package com.mingshi.skyflying.anomaly_detection.controller;

import com.mingshi.skyflying.anomaly_detection.service.impl.SegmentDetailServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/26
 */
@RestController
public class TestControllerA {
    @Autowired
    private SegmentDetailServiceImpl segmentDetailService;
    @GetMapping("test")
    public void test() {
        segmentDetailService.getYesterdaySegmentDetail();
    }
}
