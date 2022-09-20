package com.mingshi.skyflying.anomaly_detection;

import com.mingshi.skyflying.anomaly_detection.dao.CoarseSegmentDetailOnTimeMapper;
import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTableTask;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @Author: 唐郑翔
 * @Description:
 * @Date: create in 2022/8/30
 */
@Service
@Slf4j
public class AnomalyDetectionBusiness {

    @Resource
    UserPortraitByTimeTask userPortraitByTimeTask;

    @Resource
    UserPortraitByTableTask userPortraitByTableTask;

    @Resource
    RedisPoolUtil redisPoolUtil;

    @Resource
    CoarseSegmentDetailOnTimeMapper coarseSegmentDetailOnTimeMapper;

    @Resource
    MsSegmentDetailMapper segmentDetailMapper;
    @Resource
    RedissonClient redissonClient;
    @Resource
    PortraitConfigMapper portraitConfigMapper;

    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:updatePortrait";


    @Value("${anomalyDetection.redisKey.portraitByTime.prefix:anomaly_detection:portraitByTime:}")
    private String TIME_PREFIX;

    @Value("${anomalyDetection.redisKey.portraitByTable.prefix:anomaly_detection:portraitByTable:}")
    private String TABLE_PREFIX;

    private final String MORNING = "morning";

    private final String AFTERNOON = "afternoon";

    private final String NIGHT = "night";

//    //TODO: 改成可配置
//    private final double visitRate = 0.3;
//
//    //TODO: 改成可配置
//    private final int visitCount = 5;
//
//    //TODO: 改成可配置
//    private final boolean enableTimeRule = true;
//
//    //TODO: 改成可配置
//    private final boolean enableTableRule = true;

    /**
     * 判断是否告警
     */
    public void userVisitedIsAbnormal(List<MsSegmentDetailDo> segmentDetailDos, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDos) {
            if (portraitConfig.getEnableTimeRule()) {
                userVisitedTimeIsAbnormal(segmentDetailDo, msAlarmInformationDoList);
            }
            if (portraitConfig.getEnableTableRule()) {
                userVisitedTableIsAbnormal(segmentDetailDo, msAlarmInformationDoList);
            }
        }

    }

    /**
     * 判断是否告警-库表维度
     */
    public void userVisitedTableIsAbnormal(MsSegmentDetailDo segmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        String username = segmentDetailDo.getUserName();
        String dbInstance = segmentDetailDo.getDbInstance();
        String table = segmentDetailDo.getMsTableName();
        if (StringUtil.isEmpty(username) || StringUtil.isEmpty(dbInstance) || StringUtil.isEmpty(table)) return;
        List<MsSegmentDetailDo> list = new ArrayList<>();
        list.add(segmentDetailDo);
        //一条信息包含多张表名, 拆分一下
        List<MsSegmentDetailDo> msSegmentDetailDos = userPortraitByTableTask.splitTable(list);
        for (MsSegmentDetailDo segmentDetail : msSegmentDetailDos) {
            userVisitedTableIsAbnormalHandler(segmentDetail, msAlarmInformationDoList);
        }
    }

    private void userVisitedTableIsAbnormalHandler(MsSegmentDetailDo segmentDetail, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        String tableName = segmentDetail.getDbInstance() + "." + segmentDetail.getMsTableName();
        Integer count = getCountByTable(segmentDetail.getUserName(), tableName);
        if (count == null) {
            //没有用户画像
            MsAlarmInformationDo msAlarmInformationDo = doNoTablePortrait(segmentDetail, msAlarmInformationDoList);
            if (msAlarmInformationDo != null) {
                msAlarmInformationDoList.add(msAlarmInformationDo);
            }
        } else {
            //有用户画像
            if (count > portraitConfig.getRuleTableCount()) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetail, AlarmEnum.TABLE_ALARM));
            }
        }
    }

    /**
     * 没有库表用户画像
     */
    private MsAlarmInformationDo doNoTablePortrait(MsSegmentDetailDo segmentDetail, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (isNewUser(segmentDetail.getUserName())) {
            if (existNewUserAlarm(segmentDetail, msAlarmInformationDoList)) return null;
            else {
                return buildAlarmInfo(segmentDetail, AlarmEnum.NEW_USER);
            }
        }
        //老用户但是没画像
        //产生告警信息
        return buildAlarmInfo(segmentDetail, AlarmEnum.TABLE_ALARM);
    }

    /**
     * 判断是否已存在新用户告警
     */
    private boolean existNewUserAlarm(MsSegmentDetailDo segmentDetail, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        String username = segmentDetail.getUserName();
        for (MsAlarmInformationDo alarmInformationDo : msAlarmInformationDoList) {
            if (alarmInformationDo.getUserName().equals(username) && alarmInformationDo.getAlarmContent().contains("首次出现")) {
                return true;
            }
        }
        return false;
    }

    private Integer getCountByTable(String username, String tableName) {
        String redisKey = buildTableRedisKey(username, tableName);
        Object o = redisPoolUtil.get(redisKey);
        if (o == null) return null;
        return Integer.parseInt((String) o);
    }


    /**
     * 判断是否告警-时间维度
     */
    public void userVisitedTimeIsAbnormal(MsSegmentDetailDo segmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (segmentDetailDo.getUserName() == null) return;
        String userName = segmentDetailDo.getUserName();
        String time = segmentDetailDo.getStartTime();
        String interval = getInterval(time);
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        if (interval == null || interval.length() == 0) {
            log.error("userVisitedTimeIsAbnormal中提取访问记录时间失败, 具体时间为{}, globalTraceId为{}"
                    , time, segmentDetailDo.getGlobalTraceId());
            return;
        }
        Double rateByInterVal = getRateByInterVal(userName, interval);
        if (rateByInterVal == null) {
            //没有用户画像
            msAlarmInformationDoList.add(doNoTimePortrait(segmentDetailDo));
        } else {
            //有用户画像
            if (rateByInterVal < portraitConfig.getRuleTimeRate()) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetailDo, AlarmEnum.TIME_ALARM));
            }
        }
    }


    /**
     * 没有时间用户画像
     */
    private MsAlarmInformationDo doNoTimePortrait(MsSegmentDetailDo segmentDetailDo) {
        if (isNewUser(segmentDetailDo.getUserName())) {
            return buildAlarmInfo(segmentDetailDo, AlarmEnum.NEW_USER);
        }
        //老用户但是没画像(上次访问距今已超过画像统计周期)
        //产生告警信息
        return buildAlarmInfo(segmentDetailDo, AlarmEnum.TIME_ALARM);
    }

    private MsAlarmInformationDo buildAlarmInfo(MsSegmentDetailDo segmentDetailDo, AlarmEnum alarmEnum) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setUserName(segmentDetailDo.getUserName());
        msAlarmInformationDo.setOriginalTime(DateTimeUtil.strToDate(segmentDetailDo.getStartTime()));
        msAlarmInformationDo.setGlobalTraceId(segmentDetailDo.getGlobalTraceId());
        String content = "";
        if (alarmEnum == AlarmEnum.NEW_USER) {
            msAlarmInformationDo.setMatchRuleId(AlarmEnum.NEW_USER.getCode());
            content = "用户 " + segmentDetailDo.getUserName() + " 首次出现";
        }
        if (alarmEnum == AlarmEnum.TIME_ALARM) {
            msAlarmInformationDo.setMatchRuleId(AlarmEnum.TIME_ALARM.getCode());
            content = "用户" + segmentDetailDo.getUserName() + "以往在该时段很少访问";
        }
        if (alarmEnum == AlarmEnum.TABLE_ALARM) {
            msAlarmInformationDo.setMatchRuleId(AlarmEnum.TABLE_ALARM.getCode());
            content = "用户" + segmentDetailDo.getUserName() + "以往很少访问表" + segmentDetailDo.getDbInstance() + "." + segmentDetailDo.getMsTableName();
        }
        msAlarmInformationDo.setAlarmContent(content);
        return msAlarmInformationDo;
    }

    /**
     * 判断是否是新用户
     */
    private boolean isNewUser(String userName) {
        return coarseSegmentDetailOnTimeMapper.selectOneByUsername(userName) == null;
    }

    /**
     * 组装Redis的Key
     */
    private String buildTimeRedisKey(String username, String interval) {
        return TIME_PREFIX + username + ":" + interval;
    }

    private String buildTableRedisKey(String username, String key) {
        return TABLE_PREFIX + username + ":" + key;
    }

    /**
     * 判断所属时段
     */
    private String getInterval(String timeStr) {
        Pattern pattern = Pattern.compile("\\d+-\\d+-\\d+\\s+(\\d+):");
        Matcher m = pattern.matcher(timeStr);
        if (!m.find()) {
            return null;
        }
        int time = Integer.parseInt(m.group(1));
        if (time >= 5 && time < 13) {
            return MORNING;
        } else if (time >= 13 && time < 21) {
            return AFTERNOON;
        } else if ((time >= 21 && time < 24) || (time >= 0 && time < 5)) {
            return NIGHT;
        } else {
            log.error("提取时间出错, 原数据{}, 提取后{}", timeStr, time);
        }
        return null;
    }

    /**
     * 获取该用户画像所定义该时段正常访问频率
     */
    private Double getRateByInterVal(String username, String interval) {
        String redisKey = buildTimeRedisKey(username, interval);
        Object o = redisPoolUtil.get(redisKey);
        if (o == null) return null;
        return Double.parseDouble((String) o);
    }

    /**
     * 更新用户画像
     */
    public void updatePortrait() {
        RLock lock = redissonClient.getLock(REDIS_LOCK);
        lock.lock();
        try {
            // 时间维度
            userPortraitByTimeTask.updatePortrait();
            // 空间维度
            userPortraitByTableTask.updatePortrait();
        } catch (Exception e) {
            log.error("更新用户画像失败");
        } finally {
            lock.unlock();
        }
    }

    /**
     * 插入粗粒度表
     */
    public void insertCoarse(AnomalyDetectionInfoBo anomalyDetectionInfoBo) {
        MsSegmentDetailDo segmentDetail = segmentDetailMapper.selectByGlobalTraceId(anomalyDetectionInfoBo.getGlobalTraceId());
        if (segmentDetail == null) return;
        if (Objects.equals(anomalyDetectionInfoBo.getMatchRuleId(), AlarmEnum.TIME_ALARM.getCode())) {
            userPortraitByTimeTask.insertTimeCoarse(segmentDetail);
        }
        if (Objects.equals(anomalyDetectionInfoBo.getMatchRuleId(), AlarmEnum.TABLE_ALARM.getCode())) {
            userPortraitByTableTask.insertTableCoarse(segmentDetail);
        }
    }
}
