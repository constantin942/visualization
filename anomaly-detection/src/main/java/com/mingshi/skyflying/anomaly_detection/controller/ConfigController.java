package com.mingshi.skyflying.anomaly_detection.controller;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.config.InitDemoMode;
import com.mingshi.skyflying.anomaly_detection.dao.DingAlarmConfigMapper;
import com.mingshi.skyflying.anomaly_detection.dao.HighRiskOptMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmConfig;
import com.mingshi.skyflying.anomaly_detection.domain.HighRiskOpt;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.service.impl.HighRiskOptServiceImpl;
import com.mingshi.skyflying.common.aspect.OperationAuditAspectAnnotation;
import com.mingshi.skyflying.common.response.ServerResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/9/19
 */
@RestController
@Slf4j
@Validated
@RequestMapping("/api/skyflying/config")
public class ConfigController {

    @Resource
    PortraitConfigMapper portraitConfigMapper;

    @Resource
    HighRiskOptMapper highRiskOptMapper;

    @Resource
    HighRiskOptServiceImpl highRiskOptService;

    @Resource
    DingAlarmConfigMapper dingAlarmConfigMapper;

    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    @GetMapping("getConfigDic")
    public ServerResponse<ArrayList<String>> getAllConfigDic(@RequestParam String typeName) {
        return ServerResponse.createBySuccess(portraitConfigMapper.selectByName(typeName));
    }

    @GetMapping("getPortraitConfig")
    public ServerResponse<PortraitConfig> getPortraitConfig() {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        return ServerResponse.createBySuccess(portraitConfig);
    }

    @OperationAuditAspectAnnotation(isStart = true)
    @Transactional(rollbackFor = Exception.class)
    @PutMapping("updatePortraitConfig")
    public ServerResponse updatePortraitConfig(@Valid @RequestBody PortraitConfig portraitConfig) {
        PortraitConfig portraitConfigOld = portraitConfigMapper.selectOne();
        anomalyDetectionBusiness.updatePortraitOnConfig(portraitConfigOld, portraitConfig);
        portraitConfigMapper.deleteOld();
        portraitConfigMapper.insertSelective(portraitConfig);
        return ServerResponse.createBySuccess();
    }

    @GetMapping("getDemoMode")
    public ServerResponse<Boolean> getDemoMode() {
        Boolean isDemoMode = InitDemoMode.getIsDemoMode();
        return ServerResponse.createBySuccess(isDemoMode);
    }

    @PutMapping("setDemoMode")
    public ServerResponse setDemoMode(@RequestParam Boolean enable) {
        InitDemoMode.setIsDemoMode(enable);
        return ServerResponse.createBySuccess();
    }

    @GetMapping("getAllHighRiskOpt")
    public ServerResponse<List<HighRiskOpt>> getAllHighRiskOpt() {
        return ServerResponse.createBySuccess(highRiskOptMapper.selectAll());
    }

    // todo：这里禁启用某一条高危操作规则时，需要前端把所有的高危规则都传递过来？如果是这样的话，这个设计相当糟糕。正常做法是：禁启用哪一条规则，只需要把哪一条规则的id和禁启用标识传递过来即可。
    @OperationAuditAspectAnnotation(isStart = true)
    @PutMapping("updateHighRiskOpt")
    public ServerResponse updateHighRiskOpt(@RequestBody List<HighRiskOpt> highRiskOpts) {
        return ServerResponse.createBySuccess(highRiskOptService.updateHighRiskOpt(highRiskOpts));
    }

    @GetMapping("getDingConfig")
    public ServerResponse<DingAlarmConfig> getDingConfig() {
        return ServerResponse.createBySuccess(dingAlarmConfigMapper.selectOne());
    }

    @GetMapping("getDingGapDic")
    public ServerResponse<ArrayList<Integer>> getDingGapDic() {
        return ServerResponse.createBySuccess(dingAlarmConfigMapper.selectGapDic());
    }

    @OperationAuditAspectAnnotation(isStart = true)
    @PutMapping("updateDingConfig")
    public ServerResponse updateDingConfig(@RequestBody DingAlarmConfig dingAlarmConfig) {
        dingAlarmConfigMapper.updateByPrimaryKeySelective(dingAlarmConfig);
        return ServerResponse.createBySuccess();
    }
}
