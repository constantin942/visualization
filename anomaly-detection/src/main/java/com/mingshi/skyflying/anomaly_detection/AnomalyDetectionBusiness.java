package com.mingshi.skyflying.anomaly_detection;

import com.github.benmanes.caffeine.cache.Cache;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.*;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmConfig;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.service.UserPortraitRulesService;
import com.mingshi.skyflying.anomaly_detection.service.impl.HighRiskOptServiceImpl;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTableTask;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.enums.RecordEnum;
import com.mingshi.skyflying.common.exception.AiitException;
import com.mingshi.skyflying.common.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.common.kafka.producer.records.MsConsumerRecords;
import com.mingshi.skyflying.common.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
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
    @Value("${spring.kafka.producer.anomaly-detection-consume-failed-topic:11}")
    private String anomalyDetectionConsumeFailedTopic;

    @Resource
    UserPortraitByTimeTask userPortraitByTimeTask;

    @Resource
    UserPortraitByTableTask userPortraitByTableTask;

    @Resource
    RedisPoolUtil redisPoolUtil;

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

    @Resource
    AiitKafkaProducer aiitKafkaProducer;

    @Lazy
    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;
    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:updatePortrait";

    /**
     * 判断是否告警-库表维度
     */
    public void userVisitedTableIsAbnormal(List<MsSegmentDetailDo> segmentDetailDos, List<MsAlarmInformationDo> msAlarmInformationDoList
            , PortraitConfig portraitConfig, boolean isDemoMode) {
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDos) {
            String username = segmentDetailDo.getUserName();
            String dbInstance = segmentDetailDo.getDbInstance();
            String table = segmentDetailDo.getMsTableName();
            //第一次访问距今是否在画像周期内
            if (!isDemoMode && inPeriod(username, portraitConfig.getRuleTablePeriod())) {
                continue;
            }
            if (StringUtil.isEmpty(username) || StringUtil.isEmpty(dbInstance) || StringUtil.isEmpty(table)) {
                return;
            }
            List<MsSegmentDetailDo> list = new ArrayList<>();
            list.add(segmentDetailDo);
            //一条信息包含多张表名, 拆分一下
            List<MsSegmentDetailDo> msSegmentDetailDos = userPortraitByTableTask.splitTable(list);
            for (MsSegmentDetailDo segmentDetail : msSegmentDetailDos) {
                userVisitedTableIsAbnormalHandler(segmentDetail, msAlarmInformationDoList, portraitConfig);
            }
        }
    }

    /**
     * 是否为演示模式
     */
    private boolean isDemoMode() {
        //这里其实不是缓存在Redis中, 但由于只有一个数据, 比较特殊, 也先放在这个cache里
        Cache<String, String> redisLocalCache = MsCaffeineCache.getRedisLocalCache();
        if (redisLocalCache != null) {
            //从本地缓存中获取
            String s = redisLocalCache.getIfPresent(AnomalyConst.DEMO_MODE);
            if (s != null) {
                return "1".equals(s);
            }
        }
        //从数据库中获取
        String mode = portraitConfigMapper.selectOneByName(AnomalyConst.DEMO_MODE);
        if (mode == null) {
            return false;
        }
        if (redisLocalCache != null) {
            //加入本地缓存
            redisLocalCache.put(AnomalyConst.DEMO_MODE, mode);
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

    private void userVisitedTableIsAbnormalHandler(MsSegmentDetailDo segmentDetail,
                                                   List<MsAlarmInformationDo> msAlarmInformationDoList, PortraitConfig portraitConfig) {
        String tableName = segmentDetail.getDbInstance() + "." + segmentDetail.getMsTableName();
        Integer count = userPortraitByTableTask.getCountByTable(segmentDetail.getUserName(), tableName);
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
     * 判断是否告警-时间维度
     */
    public void userVisitedTimeIsAbnormal(List<MsSegmentDetailDo> segmentDetailDos, List<MsAlarmInformationDo> msAlarmInformationDoList,
                                          PortraitConfig portraitConfig, boolean isDemoMode) {
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDos) {
            userVisitedTimeIsAbnormalHelper(segmentDetailDo, msAlarmInformationDoList, portraitConfig, isDemoMode);
        }
    }

    /**
     * 判断是否告警-时间维度
     */
    public void userVisitedTimeIsAbnormalHelper(MsSegmentDetailDo segmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList
            , PortraitConfig portraitConfig, boolean isDemoMode) {
        if (segmentDetailDo.getUserName() == null) return;
        if (portraitConfig == null) {
            portraitConfig = portraitConfigMapper.selectOne();
        }
        //第一次访问距今是否在画像周期内
        String userName = segmentDetailDo.getUserName();
        if (!isDemoMode && inPeriod(userName, portraitConfig.getRuleTablePeriod())) {
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
            return AnomalyConst.MORNING;
        } else if (time >= 13 && time < 21) {
            return AnomalyConst.AFTERNOON;
        } else if ((time >= 21 && time < 24) || (time >= 0 && time < 5)) {
            return AnomalyConst.NIGHT;
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
        if (segmentDetail == null) {
            return;
        }
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
        try {
            if (null == segmentDetaiDolList || segmentDetaiDolList.isEmpty()) {
                return;
            }

            // 如果用户画像没有初始化完毕，那么将其发送到Kafka中；2022-10-17 10:30:28
            Boolean aBoolean = userPortraitInitDone(segmentDetaiDolList);
            if(Boolean.FALSE.equals(aBoolean)){
                return;
            }
            // 进行异常检测
            doUserVisitedIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.userVisitedIsAbnormal() # 进行异常检测时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：doUserVisitedIsAbnormal</B>
     * <B>概要说明：进行异常检测</B>
     *
     * @Author zm
     * @Date 2022-10-17 15:27:40
     * @Param [segmentDetaiDolList, msAlarmInformationDoList]
     * @return void
     **/
    public void doUserVisitedIsAbnormal(List<MsSegmentDetailDo> segmentDetaiDolList, List<MsAlarmInformationDo> msAlarmInformationDoList) {
        try {
            if (null == segmentDetaiDolList || segmentDetaiDolList.isEmpty()) {
                return;
            }
            Boolean enableTimeRule = getEnableRule(AnomalyConst.TIME_SUF);
            Boolean enableTableRule = getEnableRule(AnomalyConst.TABLE_SUF);
            PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
            boolean isDemoMode = isDemoMode();
            if (Boolean.TRUE.equals(enableTableRule)) {
                userVisitedTableIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList, portraitConfig, isDemoMode);
            }
            if (Boolean.TRUE.equals(enableTimeRule)) {
                userVisitedTimeIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList, portraitConfig, isDemoMode);
            }
            highRiskOptService.visitIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
            anomalyDetectionBusiness.dingAlarm(msAlarmInformationDoList);
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.doUserVisitedIsAbnormal() # 进行异常检测时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：userPortraitInitDone</B>
     * <B>概要说明：当项目启动后，如果用户画像一直没有初始化完毕，那么将待异常检测的用户行为信息发送到Kafka中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 10:32:58
     * @Param []
     **/
    private Boolean userPortraitInitDone(List<MsSegmentDetailDo> segmentDetaiDolList) {
        // 当项目启动后，如果用户画像一直没有初始化完毕，那么将待异常检测的用户行为信息发送到Kafka中。
        if (Boolean.FALSE.equals(MsCaffeineCache.getUserPortraitInitDone())) {
            try {
                MsConsumerRecords msConsumerRecords = new MsConsumerRecords(RecordEnum.MsSegmentDetailDo_Consume_Failed.getCode(), segmentDetaiDolList);
                aiitKafkaProducer.send(anomalyDetectionConsumeFailedTopic, JsonUtil.obj2String(msConsumerRecords));
                return false;
            } catch (Exception e) {
                log.error("# AnomalyDetectionBusiness.waitUserPortraitInitDone() # 开始执行异常检测，由于用户画像还没有初始化完毕，在这里将待异常检测的信息发送到Kakfa的topic = 【{}】时，出现了异常。", anomalyDetectionConsumeFailedTopic, e);
            }
        }
        return true;
    }

    /**
     * 获取规则开关
     */
    private Boolean getEnableRule(String suffix) {
        String key = AnomalyConst.RULE_PREFIX + suffix;
        // 从本地缓存读取
        if (MsCaffeineCache.getRedisLocalCache() != null) {
            String enable = MsCaffeineCache.getFromRedisLocalCache(key);
            if (enable != null) {
                return Boolean.valueOf(enable);
            }
        }
        // 从Redis中读取
        Object o = redisPoolUtil.get(key);
        if (o != null) {
            if (MsCaffeineCache.getRedisLocalCache() != null) {
                MsCaffeineCache.putIntoRedisLocalCache(key, (String) o);
            }
            return Boolean.parseBoolean((String) o);
        }
        return cacheRuleEnable(suffix);
    }

    /**
     * 从数据库查询开关存入Redis
     */
    private boolean cacheRuleEnable(String suffix) {
        UserPortraitRulesDo timeRule = userPortraitRulesMapper.selectByPrimaryKey(AnomalyConst.TIME_ID);
        UserPortraitRulesDo tableRule = userPortraitRulesMapper.selectByPrimaryKey(AnomalyConst.TABLE_ID);
        if (null != timeRule) {
            portraitRulesService.cacheRule(timeRule.getId(), timeRule.getIsDelete());
        }
        if (null != tableRule) {
            portraitRulesService.cacheRule(tableRule.getId(), tableRule.getIsDelete());
        }
        if (suffix.equals(AnomalyConst.TIME_SUF)) {
            if (timeRule != null && timeRule.getIsDelete() != null) {
                return timeRule.getIsDelete() != 1;
            } else {
                log.error("从数据库获取规则失败");
                return false;
            }
        }
        if (suffix.equals(AnomalyConst.TABLE_SUF)) {
            if (tableRule != null && tableRule.getIsDelete() != null) {
                return tableRule.getIsDelete() != 1;
            } else {
                log.error("从数据库获取规则失败");
                return false;
            }
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
            unusualVisitedData.setVisitedData(stringIntegerMap.get(AnomalyConst.TABLE_NAME));
            unusualVisitedData.setVisitedCount(Long.valueOf(String.valueOf(stringIntegerMap.get(AnomalyConst.COUNTS))));
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
            unusualVisitedData.setVisitedData(stringIntegerMap.get(AnomalyConst.TABLE_NAME));
            unusualVisitedData.setVisitedCount(Long.valueOf(String.valueOf(stringIntegerMap.get(AnomalyConst.COUNTS))));
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
    public void dingAlarm(List<MsAlarmInformationDo> msAlarmInformationDoList) {
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
            redisPoolUtil.set(redisKey, 1, (long) gap * AnomalyConst.SECONDS);
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
        redisPoolUtil.set(redisKey, 1, (long) gap * AnomalyConst.SECONDS);
        return false;
    }

    /**
     * 从Redis中获取画像信息
     */
    public void getPortraitFromRedis() {
        // 库表画像
        Map<Object, Object> map = redisPoolUtil.hmget(AnomalyConst.REDIS_TABLE_PORTRAIT_PREFIX);
        Map<String, String> strMap = map.entrySet().stream().collect(Collectors.toMap(e -> String.valueOf(e.getKey()), e -> String.valueOf(e.getValue())));
        MsCaffeineCache.putAllIntoPortraitByTableLocalCache(strMap);
        // 时间画像
        map = redisPoolUtil.hmget(AnomalyConst.REDIS_TIME_PORTRAIT_PREFIX);
        strMap = map.entrySet().stream().collect(Collectors.toMap(e -> String.valueOf(e.getKey()), e -> String.valueOf(e.getValue())));
    }
}
