package com.mingshi.skyflying.anomaly_detection.service.impl;

import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.service.ParentService;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.domain.SegmentDetailDo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/26
 */
@Service
@Slf4j
public class SegmentDetailServiceImpl implements ParentService<SegmentDetailDo, Long> {
    @Resource
    private MsSegmentDetailMapper segmentDetailMapper;

    public List<MsSegmentDetailDo> getYesterdaySegmentDetail() {
        List<MsSegmentDetailDo> coarseDetail = segmentDetailMapper.getCoarseDetail();
        return coarseDetail;

    }
}
