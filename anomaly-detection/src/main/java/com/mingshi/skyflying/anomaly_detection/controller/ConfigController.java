package com.mingshi.skyflying.anomaly_detection.controller;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.common.response.ServerResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.*;

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

    private final String DEMO_MODE = "demo_mode";

    @GetMapping("getConfigDic")
    public ServerResponse<ArrayList<String>> getAllConfigDic(@RequestParam String typeName) {
        return ServerResponse.createBySuccess(portraitConfigMapper.selectByName(typeName));
    }

    @GetMapping("getPortraitConfig")
    public ServerResponse<PortraitConfig> getPortraitConfig() {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        return ServerResponse.createBySuccess(portraitConfig);
    }


    @Transactional(rollbackFor = Exception.class)
    @PutMapping("updatePortraitConfig")
    public ServerResponse updatePortraitConfig(@Valid @RequestBody PortraitConfig portraitConfig) {
        portraitConfigMapper.deleteOld();
        portraitConfigMapper.insertSelective(portraitConfig);
        return ServerResponse.createBySuccess();
    }

    @GetMapping("getDemoMode")
    public ServerResponse<Boolean> getDemoMode() {
        String s = portraitConfigMapper.selectOneByName(DEMO_MODE);
        Boolean enable = "1".equals(s);
        return ServerResponse.createBySuccess(enable);
    }

    @PutMapping("setDemoMode")
    public ServerResponse setDemoMode(@RequestParam Boolean enable) {
        if(Boolean.TRUE.equals(enable)) {
            portraitConfigMapper.setDemoMode("1");
        } else {
            portraitConfigMapper.setDemoMode("0");
        }
        return ServerResponse.createBySuccess();
    }
}
