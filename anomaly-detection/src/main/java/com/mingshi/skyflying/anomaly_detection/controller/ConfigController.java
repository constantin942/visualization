package com.mingshi.skyflying.anomaly_detection.controller;
import com.mingshi.skyflying.anomaly_detection.dao.DicItemMapper;
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
}
