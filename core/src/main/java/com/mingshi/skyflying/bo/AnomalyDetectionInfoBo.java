package com.mingshi.skyflying.bo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.checkerframework.checker.units.qual.N;
import org.springframework.web.bind.annotation.RequestParam;

import javax.validation.constraints.NotBlank;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/18
 */

@AllArgsConstructor
@NoArgsConstructor
@Data
public class AnomalyDetectionInfoBo {

    @NotBlank(message = "id字段必传")
    Integer id;

    /**
     * 匹配规则Id
     */
    @NotBlank(message = "命中规则字段必传")
    Integer matchRuleId;

    /**
     * 告警时间
     */
    @NotBlank(message = "告警时间必传")
    String originalTime;

    @NotBlank(message = "用户名字段必传")
    String userName;

    /**
     * 告警内容
     */
    @NotBlank(message = "告警内容字段必传")
    String alarmContent;

    /**
     * 处置类型: update : 更新用户画像 delete : 删除处置信息
     */
    @NotBlank(message = "处置字段必传")
    String flag;
}
