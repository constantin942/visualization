package com.mingshi.skyflying.anomaly_detection.domain;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.Tolerate;

import java.util.Date;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/9/7
 */
@Data
@Builder
@TableName("user_portrait_by_table")
public class UserPortraitByTableDo {
    private Integer id;

    private String username;

    private String tableName;

    private Integer count;

    private Date createTime;

    private Date updateTime;

    /**
     * 兼容@Builder
     */
    @Tolerate
    public UserPortraitByTableDo() {
    }

}
