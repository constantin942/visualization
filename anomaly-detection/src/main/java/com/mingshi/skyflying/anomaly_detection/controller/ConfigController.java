package com.mingshi.skyflying.anomaly_detection.controller;

import com.mingshi.skyflying.anomaly_detection.dao.DicItemMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.DicItem;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.common.response.ServerResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
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
    DicItemMapper dicItemMapper;

    @Resource
    PortraitConfigMapper portraitConfigMapper;

    @GetMapping("getAllConfigDic")
    public ServerResponse<Map<String, ArrayList<DicItem>>> getAllConfigDic() {
        List<DicItem> configItems = dicItemMapper.selectAll();
        Map<String, ArrayList<DicItem>> map = new HashMap<>();
        for (DicItem configItem : configItems) {
            ArrayList<DicItem> list = map.getOrDefault(configItem.getName(), new ArrayList<>());
            list.add(configItem);
            map.put(configItem.getName(), list);
        }
        for (Map.Entry<String, ArrayList<DicItem>> entry : map.entrySet()) {
            entry.getValue().sort(new Comparator<DicItem>() {
                @Override
                public int compare(DicItem x, DicItem y) {
                    return x.getSort() - y.getSort();
                }
            });
        }
        return ServerResponse.createBySuccess(map);
    }

    @GetMapping("getPortraitConfig")
    public ServerResponse<PortraitConfig> getPortraitConfig() {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        return ServerResponse.createBySuccess(portraitConfig);
    }

    @PutMapping("updatePortraitConfig")
    public ServerResponse updatePortraitConfig(@Valid @RequestBody PortraitConfig portraitConfig) {
        updatePortraitConfigHelper(portraitConfig);
        return ServerResponse.createBySuccess();
    }

    @Transactional(rollbackFor = Exception.class)
    public void updatePortraitConfigHelper(PortraitConfig portraitConfig) {
        portraitConfigMapper.deleteOld();
        portraitConfigMapper.insertSelective(portraitConfig);
    }
}
