package com.mingshi.skyflying.anomaly_detection.service.impl;

import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import com.mingshi.skyflying.anomaly_detection.service.ParentService;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.domain.SegmentDetailDo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;

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

    public void createUserPortraitByTime() {
        List<MsSegmentDetailDo> segmentDetails = segmentDetailMapper.getInfoForCoarseDetail();
        List<CoarseSegmentDetailOnTimeDo> coarseSegmentDetailOnTime = getCoarseSegmentDetailOnTime(segmentDetails);
    }

    /**
     * 全量信息生成粗粒度信息
     *
     * @param segmentDetails
     * @return
     */
    private List<CoarseSegmentDetailOnTimeDo> getCoarseSegmentDetailOnTime(List<MsSegmentDetailDo> segmentDetails) {
        //每个用户对应一个数组, 数组存储每个时段的访问次数
        HashMap<String, int[]> map = new HashMap<>();
        for (MsSegmentDetailDo coarseDetail : segmentDetails) {
            String username = coarseDetail.getUserName();
            int hour = Integer.parseInt(coarseDetail.getStartTime());
            int[] counter;
            if (!map.containsKey(username)) {
                counter = new int[24];
                map.put(username, counter);
            } else {
                counter = map.get(username);
            }
            counter[hour]++;
        }
        List<CoarseSegmentDetailOnTimeDo> list = new ArrayList<>();

        for (Map.Entry<String, int[]> entry : map.entrySet()) {
            list.add(buildCoarseSegmentDetailOnTime(entry));
        }
        return list;
    }

    /**
     * 组装单个粗粒度信息
     *
     * @param entry
     * @return
     */
    private CoarseSegmentDetailOnTimeDo buildCoarseSegmentDetailOnTime(Map.Entry<String, int[]> entry) {
        String username = entry.getKey();
        int[] counter = entry.getValue();
        int sum = 0;
        for (int i = 1; i < 24; i++) {
            sum += counter[i];
        }
        return CoarseSegmentDetailOnTimeDo.builder()
                .username(username)
                .timeIntervalCount01(counter[0])
                .timeIntervalCount12(counter[1])
                .timeIntervalCount23(counter[2])
                .timeIntervalCount34(counter[3])
                .timeIntervalCount45(counter[4])
                .timeIntervalCount56(counter[5])
                .timeIntervalCount67(counter[6])
                .timeIntervalCount78(counter[7])
                .timeIntervalCount89(counter[8])
                .timeIntervalCount910(counter[9])
                .timeIntervalCount1011(counter[10])
                .timeIntervalCount1112(counter[11])
                .timeIntervalCount1213(counter[12])
                .timeIntervalCount1314(counter[13])
                .timeIntervalCount1415(counter[14])
                .timeIntervalCount1516(counter[15])
                .timeIntervalCount1617(counter[16])
                .timeIntervalCount1718(counter[17])
                .timeIntervalCount1819(counter[18])
                .timeIntervalCount1920(counter[19])
                .timeIntervalCount2021(counter[20])
                .timeIntervalCount2122(counter[21])
                .timeIntervalCount2223(counter[22])
                .timeIntervalCount2324(counter[23])
                .counts(sum)
                .build();
    }
}
