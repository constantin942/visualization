package com.aiit.skyflying.anomaly_detection.caffeine;

import com.aiit.skyflying.anomaly_detection.domain.PortraitConfig;
import lombok.Data;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/10/17
 */
@Data
public class ConfigCache {
    /**
     * 画像设置
     */
    PortraitConfig portraitConfig;
    /**
     * 基于时间检测开关
     */
    Boolean enableTimeRule;
    /**
     * 基于库表检测开关
     */
    Boolean enableTableRule;
}
