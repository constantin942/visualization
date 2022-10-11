package com.mingshi.skyflying.anomaly_detection;

import com.mingshi.skyflying.anomaly_detection.dao.*;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmConfig;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.service.UserPortraitRulesService;
import com.mingshi.skyflying.anomaly_detection.service.impl.HighRiskOptServiceImpl;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTableTask;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.exception.AiitException;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.DingUtils;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

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

    @Resource
    HighRiskOptServiceImpl highRiskOptService;

    @Resource
    UserPortraitRulesMapper userPortraitRulesMapper;

    @Resource
    UserPortraitRulesService portraitRulesService;

    @Resource
    UserPortraitByTableMapper tableMapper;

    @Resource
    DingAlarmConfigMapper dingAlarmConfigMapper;

    private static final String TABLE_NAME = "table_name";

    private static final String COUNTS = "counts";

    private String PREFIX = "anomaly_detection:enableRule:";

    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:updatePortrait";

    private final String MORNING = "morning";

    private final String AFTERNOON = "afternoon";

    private final String NIGHT = "night";

    private final String DEMO_MODE = "demo_mode";

    private static final Integer TABLE_ID = 2;

    private static final Integer TIME_ID = 1;

    private static final String TIME_SUF = "time";

    private static final String TABLE_SUF = "table";
    private static Integer SECONDS = 60;

    /**
     * 判断是否告警-库表维度
     */
    public void userVisitedTableIsAbnormal(List<MsSegmentDetailDo> segmentDetailDos, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDos) {
            String username = segmentDetailDo.getUserName();
            String dbInstance = segmentDetailDo.getDbInstance();
            String table = segmentDetailDo.getMsTableName();
            //第一次访问距今是否在画像周期内
            if (!isDemoMode() && inPeriod(username, portraitConfig.getRuleTablePeriod())) {
                continue;
            }
            if (StringUtil.isEmpty(username) || StringUtil.isEmpty(dbInstance) || StringUtil.isEmpty(table)) return;
            List<MsSegmentDetailDo> list = new ArrayList<>();
            list.add(segmentDetailDo);
            //一条信息包含多张表名, 拆分一下
            List<MsSegmentDetailDo> msSegmentDetailDos = userPortraitByTableTask.splitTable(list);
            for (MsSegmentDetailDo segmentDetail : msSegmentDetailDos) {
                userVisitedTableIsAbnormalHandler(segmentDetail, msAlarmInformationDoList);
            }
        }
    }

    /**
     * 是否为演示模式
     */
    private boolean isDemoMode() {
        String mode = portraitConfigMapper.selectOneByName(DEMO_MODE);
        if (mode == null) {
            return false;
        }
        return "1".equals(mode);
    }

    /**
     * 判断用户是否在周期内
     */
    public boolean inPeriod(String username, int period) {
        Date date = segmentDetailMapper.selectTimeGap(username);
        if (date == null) {
            return true;
        }
        int betweenDays = (int) ((new Date().getTime() - date.getTime()) / (1000 * 60 * 60 * 24) + 0.5);
        return betweenDays <= period;
    }

    private void userVisitedTableIsAbnormalHandler(MsSegmentDetailDo segmentDetail, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        String tableName = segmentDetail.getDbInstance() + "." + segmentDetail.getMsTableName();
        Integer count = getCountByTable(segmentDetail.getUserName(), tableName);
        if (count == null) {
            //没有用户画像
            MsAlarmInformationDo msAlarmInformationDo = doNoTablePortrait(segmentDetail);
            msAlarmInformationDoList.add(msAlarmInformationDo);
        } else {
            //有用户画像
            if (count < portraitConfig.getRuleTableCount()) {
                msAlarmInformationDoList.add(buildAlarmInfo(segmentDetail, AlarmEnum.TABLE_ALARM));
            }
        }
    }

    /**
     * 没有库表用户画像
     */
    private MsAlarmInformationDo doNoTablePortrait(MsSegmentDetailDo segmentDetail) {
        //老用户但是没画像
        //产生告警信息
        return buildAlarmInfo(segmentDetail, AlarmEnum.TABLE_ALARM);
    }


    /**
     * 获取某表访问次数
     */
    private Integer getCountByTable(String username, String tableName) {
        String redisKey = userPortraitByTableTask.buildRedisKey(username, tableName);
        Object o = redisPoolUtil.get(redisKey);
        if (o == null) return null;
        return Integer.parseInt((String) o);
    }


    /**
     * 判断是否告警-时间维度
     */
    public void userVisitedTimeIsAbnormal(List<MsSegmentDetailDo> segmentDetailDos, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDos) {
            userVisitedTimeIsAbnormal(segmentDetailDo, msAlarmInformationDoList, portraitConfig);
        }
    }

    /**
     * 判断是否告警-时间维度
     */
    public void userVisitedTimeIsAbnormal(MsSegmentDetailDo segmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList, PortraitConfig portraitConfig) {
        if (segmentDetailDo.getUserName() == null) return;
        if (portraitConfig == null) {
            portraitConfig = portraitConfigMapper.selectOne();
        }
        //第一次访问距今是否在画像周期内
        String userName = segmentDetailDo.getUserName();
        if (!isDemoMode() && inPeriod(userName, portraitConfig.getRuleTablePeriod())) {
            return;
        }
        String time = segmentDetailDo.getStartTime();
        String interval = getInterval(time);
        if (interval == null || interval.length() == 0) {
            log.error("userVisitedTimeIsAbnormal中提取访问记录时间失败, 具体时间为{}, globalTraceId为{}"
                    , time, segmentDetailDo.getGlobalTraceId());
            return;
        }
        Double rateByInterVal = userPortraitByTimeTask.getRateByInterVal(userName, interval);
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
        //老用户但是没画像(上次访问距今已超过画像统计周期)
        //产生告警信息
        return buildAlarmInfo(segmentDetailDo, AlarmEnum.TIME_ALARM);
    }

    /**
     * 构建告警信息
     */
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
            throw new AiitException("更新用户画像失败");
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

    public PortraitConfig getConfig() {
        return portraitConfigMapper.selectOne();
    }

    /**
     * 判断是否告警
     */
    public void userVisitedIsAbnormal(List<MsSegmentDetailDo> segmentDetaiDolList, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        if (null == segmentDetaiDolList || segmentDetaiDolList.isEmpty()) {
            return;
        }
        try {
            Boolean enableTimeRule = getEnableRule(TIME_SUF);
            Boolean enableTableRule = getEnableRule(TABLE_SUF);
            if (Boolean.TRUE.equals(enableTableRule)) {
                userVisitedTableIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
            }
            if (Boolean.TRUE.equals(enableTimeRule)) {
                userVisitedTimeIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
            }
            highRiskOptService.visitIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
            //钉钉告警
            dingAlarm(msAlarmInformationDoList);
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.userVisitedIsAbnormal() # 进行异常检测时，出现了异常。", e);
        }
    }

    /**
     * 获取规则开关
     */
    private Boolean getEnableRule(String suffix) {
        String key = PREFIX + suffix;
        Object o = redisPoolUtil.get(key);
        if (o != null) {
            return Boolean.parseBoolean((String) o);
        }
        return cacheRuleEnable(suffix);
    }

    /**
     * 从数据库查询开关存入Redis
     */
    private Boolean cacheRuleEnable(String suffix) {
        UserPortraitRulesDo timeRule = userPortraitRulesMapper.selectByPrimaryKey(TIME_ID);
        UserPortraitRulesDo tableRule = userPortraitRulesMapper.selectByPrimaryKey(TABLE_ID);
        if (null != timeRule) {
            portraitRulesService.cacheRule(timeRule.getId(), timeRule.getIsDelete());
        }
        if (null != tableRule) {
            portraitRulesService.cacheRule(tableRule.getId(), tableRule.getIsDelete());
        }
        if (suffix.equals(TIME_SUF)) {
            assert timeRule != null;
            return timeRule.getIsDelete() != 1;
        }
        if (suffix.equals(TABLE_SUF)) {
            assert tableRule != null;
            return tableRule.getIsDelete() != 1;
        }
        return false;
    }

    /**
     * 获取用户周期内常用表
     */
    public List<UserUsualAndUnusualVisitedData> getFrequentList(String userName) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        List<Map<String, String>> list = tableMapper.selectFrequntList(userName, portraitConfig.getRuleTablePeriod(), portraitConfig.getRuleTableCount());
        List<UserUsualAndUnusualVisitedData> resList = new ArrayList<>();
        for (Map<String, String> stringIntegerMap : list) {
            UserUsualAndUnusualVisitedData unusualVisitedData = new UserUsualAndUnusualVisitedData();
            unusualVisitedData.setVisitedData(stringIntegerMap.get(TABLE_NAME));
            unusualVisitedData.setVisitedCount(Long.valueOf(String.valueOf(stringIntegerMap.get(COUNTS))));
            unusualVisitedData.setUserName(userName);
            resList.add(unusualVisitedData);
        }
        return resList;
    }

    /**
     * 获取用户周期内不常用表
     */
    public List<UserUsualAndUnusualVisitedData> getUnFrequentList(String userName) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        List<Map<String, String>> list = tableMapper.selectUnFrequntList(userName, portraitConfig.getRuleTablePeriod(), portraitConfig.getRuleTableCount());
        List<UserUsualAndUnusualVisitedData> resList = new ArrayList<>();
        for (Map<String, String> stringIntegerMap : list) {
            UserUsualAndUnusualVisitedData unusualVisitedData = new UserUsualAndUnusualVisitedData();
            unusualVisitedData.setVisitedData(stringIntegerMap.get(TABLE_NAME));
            unusualVisitedData.setVisitedCount(Long.valueOf(String.valueOf(stringIntegerMap.get(COUNTS))));
            unusualVisitedData.setUserName(userName);
            resList.add(unusualVisitedData);
        }
        return resList;
    }

    public List<UserCoarseInfo> getCoarseCountsOfUsers(String username) {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        Integer period = portraitConfig.getRuleTablePeriod();
        List<String> users = tableMapper.getAllUser(username, period);
        List<UserCoarseInfo> coarseInfoList = new ArrayList<>(users.size() * 2);
        for (String user : users) {
            UserCoarseInfo userCoarseInfo = tableMapper.selectCoarseCountsOfUser(user, period);
            if (userCoarseInfo != null) {
                userCoarseInfo.setLastVisitedDate(tableMapper.getLastVisitedDate(user));
                userCoarseInfo.setVisitedCount(tableMapper.getCounts(user));
                coarseInfoList.add(userCoarseInfo);
            }
        }
        return coarseInfoList;
    }

    /**
     * 钉钉告警
     */
    private void dingAlarm(List<MsAlarmInformationDo> msAlarmInformationDoList) {
        HashSet<String> set = new HashSet<>();
        for (MsAlarmInformationDo msAlarmInformation : msAlarmInformationDoList) {
            set.add(msAlarmInformation.getMatchRuleId() + Const.POUND_KEY + msAlarmInformation.getUserName());
        }
        DingAlarmConfig dingAlarmConfig = dingAlarmConfigMapper.selectOne();
        for (String str : set) {
            dingAlarmHelper(str, dingAlarmConfig);
        }
    }

    /**
     * 告警单条消息
     */
    private void dingAlarmHelper(String redisKey, DingAlarmConfig dingAlarmConfig) {
        Integer gap = dingAlarmConfig.getGap();
        if (Boolean.FALSE.equals(isAlarmed(redisKey, gap))) {
            redisPoolUtil.set(redisKey, 1, (long) gap * SECONDS);
            String message = buildDingAlarmInfo(redisKey);
            try {
                List<String> mobiles = null;
                if (!StringUtil.isEmpty(dingAlarmConfig.getMobiles())) {
                    mobiles = Arrays.stream(dingAlarmConfig.getMobiles().split(Const.POUND_KEY)).collect(Collectors.toList());
                }
                DingUtils.dingRequest(message, dingAlarmConfig.getWebhook(), dingAlarmConfig.getSecret(), mobiles);
                log.info("钉钉告警成功");
            } catch (Exception e) {
                log.error("钉钉告警发生异常:{}", e.getMessage());
            }
        }
    }

    /**
     * 构建钉钉告警内容
     */
    private String buildDingAlarmInfo(String redisKey) {
        String[] strings = redisKey.split(Const.POUND_KEY);
        StringBuilder sb = new StringBuilder();
        sb.append("用户").append(strings[1]);
        Integer code = Integer.valueOf(strings[0]);
        if (code.equals(AlarmEnum.TIME_ALARM.getCode())) {
            sb.append("以往在该时段不经常访问");
            return sb.toString();
        }
        if (code.equals(AlarmEnum.TABLE_ALARM.getCode())) {
            sb.append("访问了不经常使用的表");
            return sb.toString();
        }
        sb.append("进行了高危操作");
        return sb.toString();
    }

    /**
     * 判断是否在告警间隔内
     */
    private boolean isAlarmed(String redisKey, Integer gap) {
        Object o = redisPoolUtil.get(redisKey);
        if (o != null) {
            return true;
        }
        redisPoolUtil.set(redisKey, 1, (long) gap * SECONDS);
        return false;
    }

}
