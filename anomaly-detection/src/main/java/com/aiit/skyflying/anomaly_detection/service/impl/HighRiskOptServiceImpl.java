package com.aiit.skyflying.anomaly_detection.service.impl;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.aiit.skyflying.anomaly_detection.dao.HighRiskOptMapper;
import com.aiit.skyflying.anomaly_detection.domain.HighRiskOpt;
import com.aiit.skyflying.common.constant.AnomalyConst;
import com.aiit.skyflying.common.domain.MsAlarmInformationDo;
import com.aiit.skyflying.common.domain.MsSegmentDetailDo;
import com.aiit.skyflying.common.enums.AlarmEnum;
import com.aiit.skyflying.common.utils.DateTimeUtil;
import com.aiit.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/9/28
 */
@Service
@Slf4j
public class HighRiskOptServiceImpl {
    @Resource
    HighRiskOptMapper highRiskOptMapper;

    private Integer highRiskSize;

    private static final Cache<String, HighRiskOpt> STRING_HIGH_RISK_OPT_CACHE = Caffeine.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .maximumSize(100)
            .build();

    /**
     * <B>方法名称：cacheHighRiskOpt</B>
     * <B>概要说明：项目启动的时候，把高危操作从MySQL中加载到本地内存中</B>
     *
     * @Author zm
     * @Date 2022-11-30 10:26:53
     * @Param []
     * @return void
     **/
    @PostConstruct
    private void cacheHighRiskOpt() {
        List<HighRiskOpt> highRiskOptList = highRiskOptMapper.selectAll();
        highRiskSize = highRiskOptList.size();
        for (HighRiskOpt highRiskOpt : highRiskOptList) {
            STRING_HIGH_RISK_OPT_CACHE.put(highRiskOpt.getKeyword(), highRiskOpt);
        }
    }

    @Transactional(rollbackFor = Exception.class)
    public Void updateHighRiskOpt(List<HighRiskOpt> highRiskOpts) {
        for (HighRiskOpt highRiskOpt : highRiskOpts) {
            if (highRiskOptMapper.updateByPrimaryKeySelective(highRiskOpt) != 1) {
                log.error("规则#{}#更新时插入数据库失败" ,highRiskOpt.getDescription());
            }
        }
        cacheHighRiskOpt(highRiskOpts);
        return null;
    }

    /**
     * <B>方法名称：cacheHighRiskOpt</B>
     * <B>概要说明：把高危操作规则放入到本地内存中，新值会覆盖老值</B>
     *
     * @Author zm
     * @Date 2022-11-30 10:35:45
     * @Param [highRiskOpts]
     * @return void
     **/
    private void cacheHighRiskOpt(List<HighRiskOpt> highRiskOpts) {
        for (HighRiskOpt highRiskOpt : highRiskOpts) {
            STRING_HIGH_RISK_OPT_CACHE.put(highRiskOpt.getKeyword(), highRiskOpt);
        }
    }

    /**
     * <B>方法名称：visitIsAbnormal</B>
     * <B>概要说明：过滤高危操作，这些高危操作指的是：
     *                          触发高危操作---修改用户密码
     *                          触发高危操作---创建用户
     *                          触发高危操作---创建了表
     *                          触发高危操作---撤销权限
     *                          触发高危操作---用户授权
     *                          触发高危操作---修改表名
     * </B>
     * @Author zm
     * @Date 2022-11-30 10:40:07
     * @Param [segmentDetailDoList, msAlarmInformationDoList]
     * @return void
     **/
    public void visitIsAbnormal(List<MsSegmentDetailDo> segmentDetailDoList, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (STRING_HIGH_RISK_OPT_CACHE.estimatedSize() != highRiskSize) {
            // 本地缓存过期，重新从数据库中加载到本地内存中；2022-11-30 10:41:41
            cacheHighRiskOpt();
        }
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDoList) {
            if (StringUtil.isEmpty(segmentDetailDo.getDbType())) {
                continue;
            }
            String optType = segmentDetailDo.getDbType().toUpperCase(Locale.ROOT).trim();
            HighRiskOpt highRiskOpt = STRING_HIGH_RISK_OPT_CACHE.getIfPresent(optType);
            // 是否启用标识 1:启用 0:不启用；2022-11-30 10:38:01
            if (null != highRiskOpt && 0 != highRiskOpt.getEnable()) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetailDo, highRiskOpt.getAlarmInfo()));
            }
        }
    }

    /**
     * <B>方法名称：buildAlarmInfo</B>
     * <B>概要说明：构造高危操作告警信息</B>
     *
     * @Author zm
     * @Date 2022-11-30 10:46:26
     * @Param [segmentDetailDo, alarmInfo]
     * @return com.aiit.skyflying.common.domain.MsAlarmInformationDo
     **/
    private MsAlarmInformationDo buildAlarmInfo(MsSegmentDetailDo segmentDetailDo, String alarmInfo) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setUserName(segmentDetailDo.getUserName() == null ? AnomalyConst.NO_USER : segmentDetailDo.getUserName());
        msAlarmInformationDo.setAlarmContent(AnomalyConst.HAVE_USER + segmentDetailDo.getUserName() + alarmInfo);
        msAlarmInformationDo.setOriginalTime(DateTimeUtil.strToDate(segmentDetailDo.getStartTime()));
        msAlarmInformationDo.setGlobalTraceId(segmentDetailDo.getGlobalTraceId());
        msAlarmInformationDo.setMatchRuleId(AlarmEnum.HIGH_RISK_OPT.getCode());
        msAlarmInformationDo.setMsTableName(segmentDetailDo.getMsTableName());
        msAlarmInformationDo.setStartTime(segmentDetailDo.getStartTime());
        msAlarmInformationDo.setDbInstance(segmentDetailDo.getDbInstance());
        return msAlarmInformationDo;
    }
}
