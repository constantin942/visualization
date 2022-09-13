package com.mingshi.skyflying.anomaly_detection.domain;

import lombok.Builder;
import lombok.Data;

import java.util.Date;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/9/7
 */
@Data
@Builder
public class UserPortraitByTableDo {
    private Integer id;

    private String username;

    private String tableName;

    private Integer count;

    private Date createTime;

    private Date updateTime;
}