package com.mingshi.skyflying.common.report;

import com.mingshi.skyflying.common.constant.Const;
import lombok.Data;

import java.util.HashMap;
import java.util.Map;

/**
 * <B>类名称：Report</B>
 * <B>概要说明：生成报告用的类</B>
 *
 * @Author zm
 * @Date 2022/12/7 08:48
 **/
@Data
public class Report {
    /**
     * 记录业务系统发送消息的时间，在生成报告中有一项统计是：统计业务系统运行的时长；2022-12-06 14:34:17
     */
    private Map<String, Map<String, String>> reportRegulatedApplicationHeartbeatMap = null;

    public Report() {
        this.reportRegulatedApplicationHeartbeatMap = new HashMap<>(Const.NUMBER_EIGHT);
    }


}
