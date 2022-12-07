package com.aiit.skyflying.common.report;

import com.aiit.skyflying.common.constant.Const;
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

    /**
     * 在单个系统中，用户总数；2022-12-07 09:57:10
     */
    private Map<String/* 系统名称 */, Map<String/* 用户名 */, Integer/* 用户访问次数 */>> singleRegulatedApplicationNumberOfUsersMap = null;

    /**
     * 在单个系统中，用户访问类型的次数；2022-12-07 16:37:58
     */
    private Map<String/* 系统名称 */, Map<String/* 用户名 */, Map<String/* SQL类型 */, Integer/* 用户访问次数 */>>> singleRegulatedApplicationUserAccessTypeMap = null;

    /**
     * 在单个系统中，被访问的总数；2022-12-07 16:02:52
     */
    private Map<String/* 系统名称 */, Integer/* 用户访问次数 */> regulatedAllOfApplicationMap = null;

    public Report() {
        this.reportRegulatedApplicationHeartbeatMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.singleRegulatedApplicationNumberOfUsersMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.regulatedAllOfApplicationMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.singleRegulatedApplicationUserAccessTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
    }


}
