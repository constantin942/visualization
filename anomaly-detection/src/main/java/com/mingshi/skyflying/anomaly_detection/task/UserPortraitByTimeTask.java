package com.mingshi.skyflying.anomaly_detection.task;

import com.github.benmanes.caffeine.cache.Cache;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.CoarseSegmentDetailOnTimeMapper;
import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTimeMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.VisitCountOnTimeInterval;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.exception.AiitException;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/29
 */
@Slf4j
@Configuration
@EnableScheduling
public class UserPortraitByTimeTask {

    @Resource
    private MsSegmentDetailMapper segmentDetailMapper;

    @Resource
    CoarseSegmentDetailOnTimeMapper coarseSegmentDetailOnTimeMapper;

    @Resource
    UserPortraitByTimeMapper userPortraitByTimeMapper;

    @Resource
    PortraitConfigMapper portraitConfigMapper;

    @Resource
    RedissonClient redissonClient;

    @Resource
    RedisPoolUtil redisPoolUtil;

    private final Integer EXPIRE = 100000;
    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:insInfo2CoarseOnTime";

    /**
     * 每日定时任务 : 全量表生成粗粒度表 -> 粗粒度表生成用户画像 -> 放入Redis
     */
    @Scheduled(cron = "0 0 2 * * ?")
    @Async
    public void createUserPortraitTask() {
        RLock lock = redissonClient.getLock(REDIS_LOCK);
        lock.lock();
        try {
            log.info("开始执行基于时间的用户画像定时任务: 全量表生成粗粒度表 -> 粗粒度表生成用户画像 -> 放入Redis");
            //1. 全量表生成粗粒度表
            insertYesterdayInfo2Coarse();
            //2. 粗粒度表生成用户画像
            //3. 放入Redis
            updatePortrait();
        } catch (Exception e) {
            log.error("生成用户画像异常");
        } finally {
            lock.unlock();
            log.info("基于时间的用户画像定时任务完成");
        }
    }

    /**
     * 放入Redis
     * key : PREFIX + username + 时段(早中晚)
     * value : 对应时段的访问频率
     */
    public void cachePortraitByTime(List<UserPortraitByTimeDo> userPortraitByTimeDos) {
        Map<String, Object> map = new HashMap<>();
        for (UserPortraitByTimeDo userPortraitByTimeDo : userPortraitByTimeDos) {
            String username = userPortraitByTimeDo.getUsername();
            String morningRate = String.valueOf(userPortraitByTimeDo.getMorningRate());
            String afternoonRate = String.valueOf(userPortraitByTimeDo.getAfternoonRate());
            String nightRate = String.valueOf(userPortraitByTimeDo.getNightRate());
            map.put(buildKey(username, AnomalyConst.MORNING), morningRate);
            map.put(buildKey(username, AnomalyConst.AFTERNOON), afternoonRate);
            map.put(buildKey(username, AnomalyConst.NIGHT), nightRate);
        }
        redisPoolUtil.hmset(AnomalyConst.REDIS_TIME_PORTRAIT_PREFIX, map);
    }

    /**
     * 组装Redis的Key
     */
    private String buildKey(String username, String interval) {
        return username + ":" + interval;
    }


    /**
     * 昨日全量信息表插入粗粒度表
     */
    public void insertYesterdayInfo2Coarse() {
        List<MsSegmentDetailDo> segmentDetails = segmentDetailMapper.getInfoForCoarseDetail();
        if (segmentDetails == null) return;
        List<CoarseSegmentDetailOnTimeDo> list = getCoarseSegmentDetailOnTime(segmentDetails);
        if (!list.isEmpty()) {
            coarseSegmentDetailOnTimeMapper.insertSelectiveBatch(list);
        }
    }

    /**
     * 获取画像周期内粗粒度表数据并生成用户画像
     */
    public List<UserPortraitByTimeDo> createUserPortraitByTime(Integer portraitByTimePeriod) {
        List<VisitCountOnTimeInterval> countOnTimeIntervalList = coarseSegmentDetailOnTimeMapper.selectInfoInPeriod(portraitByTimePeriod);
        List<UserPortraitByTimeDo> userPortraitByTimeDoList = getPortraitByCountOnTimeInterval(countOnTimeIntervalList);
        if (!userPortraitByTimeDoList.isEmpty()) {
            userPortraitByTimeMapper.insertBatch(userPortraitByTimeDoList);
        }
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
        return t.setScale(4, RoundingMode.HALF_UP).doubleValue();
    }


    /**
     * 全量信息生成粗粒度信息
     */
    public List<CoarseSegmentDetailOnTimeDo> getCoarseSegmentDetailOnTime(List<MsSegmentDetailDo> segmentDetails) {
        //每个用户对应一个数组, 数组存储每个时段的访问次数
        HashMap<String, int[]> map = new HashMap<>();
        for (MsSegmentDetailDo segmentDetail : segmentDetails) {
            String username = segmentDetail.getUserName();
            int hour = Integer.parseInt(segmentDetail.getStartTime());
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
        return buildCoarseSegmentOnTimeHelper(counter, username, sum, null);
    }

    /**
     * 构造CoarseSegmentDetailOnTimeDo类
     */
    private CoarseSegmentDetailOnTimeDo buildCoarseSegmentOnTimeHelper(int[] counter, String username, int sum, Date time) {
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
                .createTime(time)
                .build();
    }

    /**
     * 插入时间粗粒度表
     */
    public void insertTimeCoarse(MsSegmentDetailDo segmentDetailDo) {
        String username = segmentDetailDo.getUserName();
        Date time = null;
        try {
            time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(segmentDetailDo.getStartTime());
        } catch (ParseException e) {
            log.error("提取时间失败----{}", segmentDetailDo.getStartTime());
            return;
        }
        Pattern pattern = Pattern.compile("\\d+-\\d+-\\d+\\s+(\\d+):");
        Matcher m = pattern.matcher(segmentDetailDo.getStartTime());
        if (!m.find()) {
            log.error("提取时间异常{}", segmentDetailDo.getStartTime());
            throw new AiitException("提取时间异常");

        }
        int hour = Integer.parseInt(m.group(1));
        CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTime = coarseSegmentDetailOnTimeMapper.selectOneByNameAndTime(username, time);
        if (coarseSegmentDetailOnTime == null) {
            // 没有该用户当天粗粒度信息
            int[] counter = new int[24];
            counter[hour] = 1;
            CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTimeDo = buildCoarseSegmentOnTimeHelper(counter, username, 1, time);
            coarseSegmentDetailOnTimeMapper.insertSelective(coarseSegmentDetailOnTimeDo);
        } else {
            // 有该用户当天粗粒度信息
            int hours = time.getHours();
            log.info("开始插入基于时间的粗粒度表---插入前 {}", coarseSegmentDetailOnTime.getCounts());
            updateCoarseSegmentOnTime(coarseSegmentDetailOnTime, hours);
            coarseSegmentDetailOnTimeMapper.updateByPrimaryKeySelective(coarseSegmentDetailOnTime);
            log.info("完成插入基于时间的粗粒度表---插入后 {}", coarseSegmentDetailOnTime.getCounts());
        }

    }

    /**
     * 对应时间段访问次数加1
     */
    private void updateCoarseSegmentOnTime(CoarseSegmentDetailOnTimeDo coarseSegmentDetailOnTime, int hours) {
        coarseSegmentDetailOnTime.setCounts(coarseSegmentDetailOnTime.getCounts() + 1);
        switch (hours) {
            case 0:
                coarseSegmentDetailOnTime.setTimeIntervalCount01(coarseSegmentDetailOnTime.getTimeIntervalCount01() + 1);
                break;
            case 1:
                coarseSegmentDetailOnTime.setTimeIntervalCount12(coarseSegmentDetailOnTime.getTimeIntervalCount12() + 1);
                break;
            case 2:
                coarseSegmentDetailOnTime.setTimeIntervalCount23(coarseSegmentDetailOnTime.getTimeIntervalCount23() + 1);
                break;
            case 3:
                coarseSegmentDetailOnTime.setTimeIntervalCount34(coarseSegmentDetailOnTime.getTimeIntervalCount34() + 1);
                break;
            case 4:
                coarseSegmentDetailOnTime.setTimeIntervalCount45(coarseSegmentDetailOnTime.getTimeIntervalCount45() + 1);
                break;
            case 5:
                coarseSegmentDetailOnTime.setTimeIntervalCount56(coarseSegmentDetailOnTime.getTimeIntervalCount56() + 1);
                break;
            case 6:
                coarseSegmentDetailOnTime.setTimeIntervalCount67(coarseSegmentDetailOnTime.getTimeIntervalCount67() + 1);
                break;
            case 7:
                coarseSegmentDetailOnTime.setTimeIntervalCount78(coarseSegmentDetailOnTime.getTimeIntervalCount78() + 1);
                break;
            case 8:
                coarseSegmentDetailOnTime.setTimeIntervalCount89(coarseSegmentDetailOnTime.getTimeIntervalCount89() + 1);
                break;
            case 9:
                coarseSegmentDetailOnTime.setTimeIntervalCount910(coarseSegmentDetailOnTime.getTimeIntervalCount910() + 1);
                break;
            case 10:
                coarseSegmentDetailOnTime.setTimeIntervalCount1011(coarseSegmentDetailOnTime.getTimeIntervalCount1011() + 1);
                break;
            case 11:
                coarseSegmentDetailOnTime.setTimeIntervalCount1112(coarseSegmentDetailOnTime.getTimeIntervalCount1112() + 1);
                break;
            case 12:
                coarseSegmentDetailOnTime.setTimeIntervalCount1213(coarseSegmentDetailOnTime.getTimeIntervalCount1213() + 1);
                break;
            case 13:
                coarseSegmentDetailOnTime.setTimeIntervalCount1314(coarseSegmentDetailOnTime.getTimeIntervalCount1314() + 1);
                break;
            case 14:
                coarseSegmentDetailOnTime.setTimeIntervalCount1415(coarseSegmentDetailOnTime.getTimeIntervalCount1415() + 1);
                break;
            case 15:
                coarseSegmentDetailOnTime.setTimeIntervalCount1516(coarseSegmentDetailOnTime.getTimeIntervalCount1516() + 1);
                break;
            case 16:
                coarseSegmentDetailOnTime.setTimeIntervalCount1617(coarseSegmentDetailOnTime.getTimeIntervalCount1617() + 1);
                break;
            case 17:
                coarseSegmentDetailOnTime.setTimeIntervalCount1718(coarseSegmentDetailOnTime.getTimeIntervalCount1718() + 1);
                break;
            case 18:
                coarseSegmentDetailOnTime.setTimeIntervalCount1819(coarseSegmentDetailOnTime.getTimeIntervalCount1819() + 1);
                break;
            case 19:
                coarseSegmentDetailOnTime.setTimeIntervalCount1920(coarseSegmentDetailOnTime.getTimeIntervalCount1920() + 1);
                break;
            case 20:
                coarseSegmentDetailOnTime.setTimeIntervalCount2021(coarseSegmentDetailOnTime.getTimeIntervalCount2021() + 1);
                break;
            case 21:
                coarseSegmentDetailOnTime.setTimeIntervalCount2122(coarseSegmentDetailOnTime.getTimeIntervalCount2122() + 1);
                break;
            case 22:
                coarseSegmentDetailOnTime.setTimeIntervalCount2223(coarseSegmentDetailOnTime.getTimeIntervalCount2223() + 1);
                break;
            case 23:
                coarseSegmentDetailOnTime.setTimeIntervalCount2324(coarseSegmentDetailOnTime.getTimeIntervalCount2324() + 1);
                break;
            default:
                break;
        }
    }

    /**
     * 更新用户画像
     */
    public void updatePortrait() {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        //2. 粗粒度表生成用户画像
        List<UserPortraitByTimeDo> userPortraitByTimeDos = createUserPortraitByTime(portraitConfig.getRuleTimePeriod());
        //3. 放入Redis
        cachePortraitByTime(userPortraitByTimeDos);
    }

    /**
     * 获取该用户画像所定义该时段正常访问频率
     */
    public Double getRateByInterVal(String username, String interval) {
        String redisKey = buildKey(username, interval);
        Cache<String, String> redisLocalCache = MsCaffeineCache.getRedisLocalCache();
        // 从本地缓存读取
        if (redisLocalCache != null) {
            String s = redisLocalCache.getIfPresent(redisKey);
            if (s != null) {
                return Double.parseDouble(s);
            }
        }
        // 从Redis读取
        Object o = redisPoolUtil.get(redisKey);
        if (o == null) {
            updatePortrait();
        }
        o = redisPoolUtil.get(redisKey);
        if(o == null) {
            // redis中也没有
            return null;
        }
        if (redisLocalCache != null) {
            redisLocalCache.put(redisKey, (String) o);
        }
        return Double.parseDouble((String) o);
    }

    /**
     * 获取用户访问频率
     */
    public Map<String, Double> getVisitRate(String username) {
        Map<String, Double> map = new HashMap<>();
        Double morningRate = getRateByInterVal(username, AnomalyConst.MORNING);
        Double afternoonRate = getRateByInterVal(username, AnomalyConst.AFTERNOON);
        Double nightRate = getRateByInterVal(username, AnomalyConst.NIGHT);
        morningRate = morningRate == null ? 0.33 : morningRate;
        afternoonRate = afternoonRate == null ? 0.33 : afternoonRate;
        nightRate = nightRate == null ? 0.33 : nightRate;
        map.put("morning", morningRate);
        map.put("afternoon", afternoonRate);
        map.put("night", nightRate);
        return map;
    }
}
