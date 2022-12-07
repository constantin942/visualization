package com.aiit.skyflying.anomaly_detection.controller;

import com.aiit.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.aiit.skyflying.anomaly_detection.config.InitDemoMode;
import com.aiit.skyflying.anomaly_detection.dao.DingAlarmConfigMapper;
import com.aiit.skyflying.anomaly_detection.dao.HighRiskOptMapper;
import com.aiit.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.aiit.skyflying.anomaly_detection.domain.DingAlarmConfig;
import com.aiit.skyflying.anomaly_detection.domain.HighRiskOpt;
import com.aiit.skyflying.anomaly_detection.domain.PortraitConfig;
import com.aiit.skyflying.anomaly_detection.service.impl.HighRiskOptServiceImpl;
import com.aiit.skyflying.common.aspect.OperationAuditAspectAnnotation;
import com.aiit.skyflying.common.response.ServerResponse;
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
        if ( 1 != dingAlarmConfigMapper.updateByPrimaryKeySelective(dingAlarmConfig)) {
            dingAlarmConfigMapper.insertSelective(dingAlarmConfig);
        }
        return ServerResponse.createBySuccess();
    }
}
