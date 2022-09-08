package com.mingshi.skyflying.anomaly_detection;

import com.mingshi.skyflying.anomaly_detection.dao.CoarseSegmentDetailOnTimeMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
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
    RedisPoolUtil redisPoolUtil;

    @Resource
    CoarseSegmentDetailOnTimeMapper coarseSegmentDetailOnTimeMapper;

    @Value("${anomalyDetection.redisKey.portraitByTime.prefix:anomaly_detection:portraitByTime:}")
    private  String PREFIX;

    private final String MORNING = "morning";

    private final String AFTERNOON = "afternoon";

    private final String NIGHT = "night";

    //TODO: 改成可配置
    private final double rate = 0.3;

    /**
     * 判断是否告警
     */
    public void userVisitedTimeIsAbnormal(MsSegmentDetailDo segmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (segmentDetailDo.getUserName() == null) return;
        String userName = segmentDetailDo.getUserName();
        String time = segmentDetailDo.getStartTime();
        String interval = getInterval(time);
        if (interval == null || interval.length() == 0) {
            log.error("userVisitedTimeIsAbnormal中提取访问记录时间失败, 具体时间为{}, globalTraceId为{}"
                    , time, segmentDetailDo.getGlobalTraceId());
            return;
        }
        Double rateByInterVal = getRateByInterVal(userName, interval);
        if (rateByInterVal == null) {
            //没有用户画像
            msAlarmInformationDoList.add(doNoUserPortrait(segmentDetailDo));
        } else {
            //有用户画像
            if (rateByInterVal < rate) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetailDo, AlarmEnum.TIME_ALARM));
            }
        }
    }


    /**
     * 没有用户画像
     */
    private MsAlarmInformationDo doNoUserPortrait(MsSegmentDetailDo segmentDetailDo) {
        if (isNewUser(segmentDetailDo.getUserName())) {
            return buildAlarmInfo(segmentDetailDo, AlarmEnum.NEW_USER);
        }
        //老用户但是没画像(上次访问距今已超过画像统计周期)
        //1. 插入粗粒度表
        List<MsSegmentDetailDo> list = new ArrayList<>();
        List<CoarseSegmentDetailOnTimeDo> coarseSegmentDetailOnTime = userPortraitByTimeTask.getCoarseSegmentDetailOnTime(list);
        coarseSegmentDetailOnTimeMapper.insertSelectiveBatch(coarseSegmentDetailOnTime);
        //2. 产生告警信息
        return buildAlarmInfo(segmentDetailDo, AlarmEnum.TIME_ALARM);
    }

    private MsAlarmInformationDo buildAlarmInfo(MsSegmentDetailDo segmentDetailDo, AlarmEnum alarmEnum) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setUserName(segmentDetailDo.getUserName());
        msAlarmInformationDo.setMatchRuleId(AnomylyDetectionSingletonByVisitedTableEveryday.getVisitedTableRuleId());
        msAlarmInformationDo.setOriginalTime(DateTimeUtil.strToDate(segmentDetailDo.getStartTime()));
        msAlarmInformationDo.setGlobalTraceId(segmentDetailDo.getGlobalTraceId());
        String content = "";
        if (alarmEnum == AlarmEnum.NEW_USER) {
            content = "用户 " + segmentDetailDo.getUserName() + " 首次出现";
        }
        if (alarmEnum == AlarmEnum.TIME_ALARM) {
            content = "用户" + segmentDetailDo.getUserName() + "以往在该时段很少访问";
        }
        if (alarmEnum == AlarmEnum.TABLE_ALARM) {
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
    private String buildRedisKey(String username, String interval) {
        return PREFIX + username + ":" + interval;
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
        String redisKey = buildRedisKey(username, interval);
        Object o = redisPoolUtil.get(redisKey);
        if(o == null)   return null;
        return Double.parseDouble((String) o);
    }
}
