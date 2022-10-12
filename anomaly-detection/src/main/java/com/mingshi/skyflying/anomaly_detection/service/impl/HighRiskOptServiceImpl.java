package com.mingshi.skyflying.anomaly_detection.service.impl;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.mingshi.skyflying.anomaly_detection.dao.HighRiskOptMapper;
import com.mingshi.skyflying.anomaly_detection.domain.HighRiskOpt;
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

    private  Integer HIGH_RISK_SIZE;

    private static final String NO_USER = "未知用户";

    private static final String HAVE_USER = "用户";

    private static final Cache<String, HighRiskOpt> cache = Caffeine.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .maximumSize(100)
            .build();

    @PostConstruct
    private void cacheHighRiskOpt() {
        List<HighRiskOpt> highRiskOpts = highRiskOptMapper.selectAll();
        HIGH_RISK_SIZE = highRiskOpts.size();
        for (HighRiskOpt highRiskOpt : highRiskOpts) {
            cache.put(highRiskOpt.getKeyword(), highRiskOpt);
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
            cache.put(highRiskOpt.getKeyword(), highRiskOpt);
        }
    }

    public void visitIsAbnormal(List<MsSegmentDetailDo> segmentDetailDoList, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (cache.estimatedSize() != HIGH_RISK_SIZE) {
            // 有缓存过期
            cacheHighRiskOpt();
        }
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDoList) {
            if (StringUtil.isEmpty(segmentDetailDo.getDbType())) {
                continue;
            }
            String optType = segmentDetailDo.getDbType().toUpperCase(Locale.ROOT).trim();
            HighRiskOpt highRiskOpt = cache.getIfPresent(optType);
            if (highRiskOpt != null && highRiskOpt.getEnable() != 0) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetailDo, highRiskOpt.getAlarmInfo()));
            }
        }
    }

    private MsAlarmInformationDo buildAlarmInfo(MsSegmentDetailDo segmentDetailDo, String alarmInfo) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setUserName(segmentDetailDo.getUserName() == null ? NO_USER : segmentDetailDo.getUserName());
        msAlarmInformationDo.setAlarmContent(HAVE_USER + segmentDetailDo.getUserName() + alarmInfo);
        msAlarmInformationDo.setOriginalTime(DateTimeUtil.strToDate(segmentDetailDo.getStartTime()));
        msAlarmInformationDo.setGlobalTraceId(segmentDetailDo.getGlobalTraceId());
        msAlarmInformationDo.setMatchRuleId(AlarmEnum.HIGH_RISK_OPT.getCode());
        return msAlarmInformationDo;
    }
}
