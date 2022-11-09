package com.mingshi.skyflying.anomaly_detection.service.impl;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.mingshi.skyflying.anomaly_detection.dao.HighRiskOptMapper;
import com.mingshi.skyflying.anomaly_detection.domain.HighRiskOpt;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
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
public class HighRiskOptServiceImpl {
    @Resource
    HighRiskOptMapper highRiskOptMapper;

    private Integer highRiskSize;

    private static final Cache<String, HighRiskOpt> STRING_HIGH_RISK_OPT_CACHE = Caffeine.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .maximumSize(100)
            .build();

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
            highRiskOptMapper.updateByPrimaryKeySelective(highRiskOpt);
        }
        cacheHighRiskOpt(highRiskOpts);
        return null;
    }

    private void cacheHighRiskOpt(List<HighRiskOpt> highRiskOpts) {
        for (HighRiskOpt highRiskOpt : highRiskOpts) {
            STRING_HIGH_RISK_OPT_CACHE.put(highRiskOpt.getKeyword(), highRiskOpt);
        }
    }

    public void visitIsAbnormal(List<MsSegmentDetailDo> segmentDetailDoList, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (STRING_HIGH_RISK_OPT_CACHE.estimatedSize() != highRiskSize) {
            // 有缓存过期
            cacheHighRiskOpt();
        }
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDoList) {
            if (StringUtil.isEmpty(segmentDetailDo.getDbType())) {
                continue;
            }
            String optType = segmentDetailDo.getDbType().toUpperCase(Locale.ROOT).trim();
            HighRiskOpt highRiskOpt = STRING_HIGH_RISK_OPT_CACHE.getIfPresent(optType);
            if (highRiskOpt != null && highRiskOpt.getEnable() != 0) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetailDo, highRiskOpt.getAlarmInfo()));
            }
        }
    }

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
