package com.mingshi.skyflying.anomaly_detection.task;

import com.mingshi.skyflying.anomaly_detection.dao.CoarseSegmentDetailOnTimeMapper;
import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTimeMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.VisitCountOnTimeInterval;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/29
 */
@Slf4j
@Component
public class UserPortraitByTimeTask {

    @Resource
    private MsSegmentDetailMapper segmentDetailMapper;

    @Resource
    CoarseSegmentDetailOnTimeMapper coarseSegmentDetailOnTimeMapper;

    @Resource
    UserPortraitByTimeMapper userPortraitByTimeMapper;

    @Resource
    RedissonClient redissonClient;

    @Resource
    RedisPoolUtil redisPoolUtil;


    private final Integer portraitByTimePeriod = 30;

    @Value("${anomalyDetection.redisKey.portraitByTime.prefix:anomaly_detection:portraitByTime:}")
    private String PREFIX;

    private final String MORNING = "morning";

    private final String AFTERNOON = "afternoon";

    private final String NIGHT = "night";

    private final Integer EXPIRE = 100000;

    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:insInfo2CoarseOnTime";

    /**
     * 每日定时任务 : 全量表生成粗粒度表 -> 粗粒度表生成用户画像 -> 放入Redis
     */
    private void createUserPortraitTask() {
        RLock lock = redissonClient.getLock(REDIS_LOCK);
        lock.lock();
        try {
            //1. 全量表生成粗粒度表
            insertYesterdayInfo2Coarse();
            //2. 粗粒度表生成用户画像
            List<UserPortraitByTimeDo> userPortraitByTimeDos = createUserPortraitByTime(portraitByTimePeriod);
            //3. 放入Redis
            cachePortraitByTime(userPortraitByTimeDos);
        } catch (Exception e) {
            log.error("生成用户画像异常");
        } finally {
            lock.unlock();
        }
    }

    /**
     * 放入Redis
     * key : PREFIX + username + 时段(早中晚)
     * value : 对应时段的访问频率
     */
    public void cachePortraitByTime(List<UserPortraitByTimeDo> userPortraitByTimeDos) {
        for (UserPortraitByTimeDo userPortraitByTimeDo : userPortraitByTimeDos) {
            String username = userPortraitByTimeDo.getUsername();
            String morningRate = String.valueOf(userPortraitByTimeDo.getMorningRate());
            String afternoonRate = String.valueOf(userPortraitByTimeDo.getAfternoonRate());
            String nightRate = String.valueOf(userPortraitByTimeDo.getNightRate());
            redisPoolUtil.set(buildRedisKey(username, MORNING), morningRate, EXPIRE);
            redisPoolUtil.set(buildRedisKey(username, AFTERNOON), afternoonRate, EXPIRE);
            redisPoolUtil.set(buildRedisKey(username, NIGHT), nightRate, EXPIRE);
        }
    }

    /**
     * 组装Redis的Key
     */
    private String buildRedisKey(String username, String interval) {
        return PREFIX + username + ":" + interval;
    }

    /**
     * 昨日全量信息表插入粗粒度表
     */
    public void insertYesterdayInfo2Coarse() {
        List<MsSegmentDetailDo> segmentDetails = segmentDetailMapper.getInfoForCoarseDetail();
        List<CoarseSegmentDetailOnTimeDo> list = getCoarseSegmentDetailOnTime(segmentDetails);
        coarseSegmentDetailOnTimeMapper.insertSelectiveBatch(list);
    }

    /**
     * 获取画像周期内粗粒度表数据并生成用户画像
     */
    public List<UserPortraitByTimeDo> createUserPortraitByTime(Integer portraitByTimePeriod) {
        List<VisitCountOnTimeInterval> countOnTimeIntervalList = coarseSegmentDetailOnTimeMapper.selectInfoInPeriod(portraitByTimePeriod);
        List<UserPortraitByTimeDo> userPortraitByTimeDoList = getPortraitByCountOnTimeInterval(countOnTimeIntervalList);
        userPortraitByTimeMapper.insertBatch(userPortraitByTimeDoList);
        return userPortraitByTimeDoList;
    }

    /**
     * 统计List<VisitCountOnTimeInterval>, 生成List<UserPortraitByTimeDo>
     */
    private List<UserPortraitByTimeDo> getPortraitByCountOnTimeInterval(List<VisitCountOnTimeInterval> countOnTimeIntervalList) {
        List<UserPortraitByTimeDo> userPortraitByTimeDos = new ArrayList<>();
        for (VisitCountOnTimeInterval visitCountOnTimeInterval : countOnTimeIntervalList) {
            double counts = 1.0 * visitCountOnTimeInterval.getCounts();
            userPortraitByTimeDos.add(UserPortraitByTimeDo.builder()
                    .username(visitCountOnTimeInterval.getUsername())
                    .morningRate(calAccuracy(visitCountOnTimeInterval.getMorningCount() / counts))
                    .afternoonRate(calAccuracy(visitCountOnTimeInterval.getAfternoonCount() / counts))
                    .nightRate(calAccuracy(visitCountOnTimeInterval.getNightCount() / counts))
                    .build());
        }
        return userPortraitByTimeDos;
    }

    private double calAccuracy(double value) {
        BigDecimal t = BigDecimal.valueOf(value);
        return t.setScale(3, RoundingMode.HALF_UP).doubleValue();
    }


    /**
     * 全量信息生成粗粒度信息
     */
    public List<CoarseSegmentDetailOnTimeDo> getCoarseSegmentDetailOnTime(List<MsSegmentDetailDo> segmentDetails) {
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
     */
    private CoarseSegmentDetailOnTimeDo buildCoarseSegmentDetailOnTime(Map.Entry<String, int[]> entry) {
        String username = entry.getKey();
        int[] counter = entry.getValue();
        int sum = 0;
        for (int i = 1; i < 24; i++) {
            sum += counter[i];
        }
        return CoarseSegmentDetailOnTimeDo
                .builder()
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
