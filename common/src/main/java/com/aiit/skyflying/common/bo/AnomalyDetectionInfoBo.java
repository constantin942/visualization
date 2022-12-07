package com.aiit.skyflying.common.bo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;


/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/18
 */

@AllArgsConstructor
@NoArgsConstructor
@Data
public class AnomalyDetectionInfoBo {

    @NotNull(message = "id字段必传")
    Integer id;

    /**
     * 匹配规则Id
     */
    @NotNull(message = "命中规则字段必传")
    Integer matchRuleId;

    /**
     * 告警时间
     */
    @NotNull(message = "告警时间必传")
    String originalTime;

    @NotNull(message = "用户名字段必传")
    String userName;

    /**
     * 告警内容
     */
    @NotNull(message = "告警内容字段必传")
    String alarmContent;

    /**
     * 处置类型: update : 更新用户画像 delete : 删除处置信息
     */
    @NotNull(message = "处置字段必传")
    String flag;

    /**
     * 全局跟踪id
     */
    @NotNull(message = "全局跟踪id必传")
    private String globalTraceId;

    /**
     * 是否更新画像
     */
    @NotNull(message = "是否更新画像字段未传")
    private Integer updateUserPortrait;
}
