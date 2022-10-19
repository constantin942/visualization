package com.mingshi.skyflying.anomaly_detection.controller;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTableTask;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/26
 */
@RestController
@RequestMapping("test")
public class TestControllerA {

    @Resource
    UserPortraitByTimeTask timeTask;

    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    @Resource
    UserPortraitByTableTask tableTask;

    @Resource
    RedisPoolUtil redisPoolUtil;

    @GetMapping("period")
    public void testPeriod() {
        anomalyDetectionBusiness.inPeriod("admin", 15);
    }


    @GetMapping("alarm")
    public void testAlarm(@RequestParam(value = "name") String name) {
        MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
        msSegmentDetailDo.setUserName(name);
        msSegmentDetailDo.setStartTime("2022-09-06 09:04:04");
        msSegmentDetailDo.setGlobalTraceId("94d2c1159fd44d06b81f4bcc1a35ee2f.43.16577678448421029");
        msSegmentDetailDo.setDbInstance("zhejiang_mobile");
        msSegmentDetailDo.setDbType("alter");
        msSegmentDetailDo.setMsTableName("sys_operator_role");
        List<MsAlarmInformationDo> msAlarmInformationDoList = new ArrayList<>();
        List<MsSegmentDetailDo> list = new ArrayList<>();
        list.add(msSegmentDetailDo);
        anomalyDetectionBusiness.userVisitedIsAbnormal(list);
        System.out.println(msAlarmInformationDoList.get(0));
    }

    @GetMapping("testInsertYesterdayInfo2Portrait")
    public void testInsertYesterdayInfo2Portrait() {
        tableTask.insertYesterdayInfo2Portrait();
    }

    @GetMapping("cachePortraitByTable")
    public void cachePortraitByTable() {
        tableTask.cachePortraitByTable();
    }


    @GetMapping("getFrequentList")
    public void getFrequentList(){
        anomalyDetectionBusiness.getFrequentList("admin");
    }

    @GetMapping("createPortrait")
    public void createPortrait() {
        tableTask.createUserPortraitTask();
        timeTask.createUserPortraitTask();
    }

    @GetMapping("fuzGet")
    public void fuzGet(@RequestParam(value = "prefix")String prefix) {
        Map<String, String> stringStringMap = redisPoolUtil.fuzGet(prefix);
        System.out.println(stringStringMap.size());

    }

    @GetMapping("updatePor")
    public void updatePor() {
        anomalyDetectionBusiness.updatePortrait();
    }
}
