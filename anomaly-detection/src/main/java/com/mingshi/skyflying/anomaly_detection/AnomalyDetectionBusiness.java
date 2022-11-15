package com.mingshi.skyflying.anomaly_detection;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.config.InitDemoMode;
import com.mingshi.skyflying.anomaly_detection.dao.DingAlarmInformationMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTableMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitRulesMapper;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmInformation;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.service.UserPortraitRulesService;
import com.mingshi.skyflying.anomaly_detection.service.impl.HighRiskOptServiceImpl;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTableTask;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.dao.MsAlarmInformationMapper;
import com.mingshi.skyflying.common.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.enums.RecordEnum;
import com.mingshi.skyflying.common.exception.AiitException;
import com.mingshi.skyflying.common.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.common.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.common.kafka.producer.records.MsConsumerRecords;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
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
    @Value("${spring.kafka.anomaly-detection-consume-failed-topic}")
    private String anomalyDetectionConsumeFailedTopic;

    @Value("${spring.kafka.anomaly-detection-alarm-topic}")
    private String anomalyDetectionAlarmTopic;

    @Resource
    UserPortraitByTimeTask userPortraitByTimeTask;

    @Resource
    UserPortraitByTableTask userPortraitByTableTask;

    @Resource
    RedisPoolUtil redisPoolUtil;

    @Resource
    MsSegmentDetailDao msSegmentDetailDao;

    @Resource
    MsAlarmInformationMapper alarmInformationMapper;

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
    DingAlarmInformationMapper dingAlarmInformationMapper;

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
    public void userVisitedTableIsAbnormal(List<MsSegmentDetailDo> segmentDetailDoList, List<MsAlarmInformationDo> msAlarmInformationDoList, PortraitConfig portraitConfig, boolean isDemoMode) {
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDoList) {
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
     * 判断用户是否在周期内
     */
    public boolean inPeriod(String username, int period) {
        Date date = MsCaffeineCache.getFromFirstVisitTime(username);
        if (date == null) {
            return true;
        }
        int betweenDays = (int) ((System.currentTimeMillis() - date.getTime()) / (1000 * 60 * 60 * 24) + 0.5);
        return betweenDays <= period;
    }

    private void userVisitedTableIsAbnormalHandler(MsSegmentDetailDo msSegmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList, PortraitConfig portraitConfig) {
        String tableName = msSegmentDetailDo.getDbInstance() + "." + msSegmentDetailDo.getMsTableName();
        Integer count = userPortraitByTableTask.getCountByTable(msSegmentDetailDo.getUserName(), tableName);
        if (null == count) {
            //没有用户画像
            MsAlarmInformationDo msAlarmInformationDo = doNoTablePortrait(msSegmentDetailDo);
            msAlarmInformationDoList.add(msAlarmInformationDo);
        } else if (count < portraitConfig.getRuleTableCount()) {
            //有用户画像
            msAlarmInformationDoList.add(buildAlarmInfo(msSegmentDetailDo, AlarmEnum.TABLE_ALARM));
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
    public void userVisitedTimeIsAbnormal(List<MsSegmentDetailDo> segmentDetailDos, List<MsAlarmInformationDo> msAlarmInformationDoList, PortraitConfig portraitConfig, boolean isDemoMode) {
        for (MsSegmentDetailDo segmentDetailDo : segmentDetailDos) {
            userVisitedTimeIsAbnormalHelper(segmentDetailDo, msAlarmInformationDoList, portraitConfig, isDemoMode);
        }
    }

    /**
     * 判断是否告警-时间维度
     */
    public void userVisitedTimeIsAbnormalHelper(MsSegmentDetailDo segmentDetailDo, List<MsAlarmInformationDo> msAlarmInformationDoList, PortraitConfig portraitConfig, boolean isDemoMode) {
        if (segmentDetailDo.getUserName() == null) {
            return;
        }
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
            log.error("userVisitedTimeIsAbnormal中提取访问记录时间失败, 具体时间为{}, globalTraceId为{}", time, segmentDetailDo.getGlobalTraceId());
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
        msAlarmInformationDo.setMsTableName(segmentDetailDo.getMsTableName());
        msAlarmInformationDo.setStartTime(segmentDetailDo.getStartTime());
        msAlarmInformationDo.setDbInstance(segmentDetailDo.getDbInstance());
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
        Pattern pattern = Pattern.compile(Const.PATTERN);
        Matcher m = pattern.matcher(timeStr);
        if (!m.find()) {
            return null;
        }
        int time = Integer.parseInt(m.group(1));
        boolean morning = (time >= Const.NUM_FIVE && time < Const.NUM_THIRTEEN);
        boolean afternoon = (time >= Const.NUM_THIRTEEN && time < Const.NUM_TWENTY_ONE);
        boolean night = (time >= Const.NUM_TWENTY_ONE && time < Const.NUM_TWENTY_FOUR) || (time >= Const.NUMBER_ZERO && time < Const.NUM_FIVE);
        if (morning) {
            return AnomalyConst.MORNING;
        } else if (afternoon) {
            return AnomalyConst.AFTERNOON;
        } else if (night) {
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
            log.error("更新用户画像失败", e);
            throw new AiitException("更新用户画像失败");
        } finally {
            lock.unlock();
        }
    }

    /**
     * 更新用户画像
     */
    @Async
    public void updatePortraitOnConfig(PortraitConfig portraitConfigOld, PortraitConfig portraitConfig) {
        if (!portraitConfigOld.getRuleTimePeriod().equals(portraitConfig.getRuleTimePeriod()) ||
            !portraitConfigOld.getRuleTablePeriod().equals(portraitConfig.getRuleTablePeriod())) {
            updatePortrait();
        }
    }

    /**
     * 插入粗粒度表
     */
    public void insertCoarse(AnomalyDetectionInfoBo anomalyDetectionInfoBo) {
        MsAlarmInformationDo alarmInformationDo = alarmInformationMapper.selectByGlobalTraceId(anomalyDetectionInfoBo.getGlobalTraceId());
        if (alarmInformationDo == null) {
            return;
        }
        if (Objects.equals(anomalyDetectionInfoBo.getMatchRuleId(), AlarmEnum.TIME_ALARM.getCode())) {
            userPortraitByTimeTask.insertTimeCoarse(alarmInformationDo);
        }
        if (Objects.equals(anomalyDetectionInfoBo.getMatchRuleId(), AlarmEnum.TABLE_ALARM.getCode())) {
            userPortraitByTableTask.insertTableCoarse(alarmInformationDo);
        }
    }

    public PortraitConfig getConfig() {
        return portraitConfigMapper.selectOne();
    }

    /**
     * 判断是否告警
     */
    public Boolean userVisitedIsAbnormal(List<MsSegmentDetailDo> segmentDetaiDolList) {
        try {
            // 当项目启动后，如果用户画像没有初始化完毕，那么将待异常检测的用户行为信息发送到Kafka中
            Boolean aBoolean = userPortraitInitNotDone(segmentDetaiDolList);
            if (Boolean.FALSE.equals(aBoolean)) {
                // 用户画像初始化失败，将消息发送到Kakfa中；2022-10-19 11:17:09
                return aBoolean;
            }
            // 用户画像已初始化完毕，现在进行异常检测；
            doUserVisitedIsAbnormal(segmentDetaiDolList);
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.userVisitedIsAbnormal() # 进行异常检测时，出现了异常。", e);
        }
        return Boolean.TRUE;
    }

    /**
     * <B>方法名称：doUserVisitedIsAbnormal</B>
     * <B>概要说明：进行异常检测</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 15:27:40
     * @Param [segmentDetaiDolList, msAlarmInformationDoList]
     **/
    public void doUserVisitedIsAbnormal(List<MsSegmentDetailDo> segmentDetaiDolList) {
        try {
            if (null == segmentDetaiDolList || segmentDetaiDolList.isEmpty()) {
                return;
            }

            Boolean enableTableRule = MsCaffeineCache.getEnableTableRule();
            // 做健壮性的判断；2022-10-19 14:13:25
            boolean noTableRule = (null == enableTableRule || (!Boolean.TRUE.equals(enableTableRule) && !Boolean.FALSE.equals(enableTableRule)));
            if (noTableRule) {
                log.error("# AnomalyDetectionBusiness.doUserVisitedIsAbnormal() # 要进行异常检测了，从本地缓存中没有获取到规则 enableTableRule 标识。将用户画像置为初始化失败，那么将待检测的消息发送到Kafka中。");
                MsCaffeineCache.setUserPortraitInitDone(Boolean.FALSE);
                userPortraitInitNotDone(segmentDetaiDolList);
                return;
            }
            Boolean enableTimeRule = MsCaffeineCache.getEnableTimeRule();
            boolean noTimeRule = (null == enableTimeRule || (!Boolean.TRUE.equals(enableTimeRule) && !Boolean.FALSE.equals(enableTimeRule)));
            if (noTimeRule) {
                log.error("# AnomalyDetectionBusiness.doUserVisitedIsAbnormal() # 要进行异常检测了，从本地缓存中没有获取到规则 enableTimeRule 标识。将用户画像置为初始化失败，那么将待检测的消息发送到Kafka中。");
                MsCaffeineCache.setUserPortraitInitDone(Boolean.FALSE);
                userPortraitInitNotDone(segmentDetaiDolList);
                return;
            }

            PortraitConfig portraitConfig = MsCaffeineCache.getPortraitConfig();
            if (null == portraitConfig) {
                log.error("# AnomalyDetectionBusiness.doUserVisitedIsAbnormal() # 要进行异常检测了，从本地缓存中没有获取到规则的配置信息。将用户画像置为初始化失败，那么将待检测的消息发送到Kafka中。");
                MsCaffeineCache.setUserPortraitInitDone(Boolean.FALSE);
                userPortraitInitNotDone(segmentDetaiDolList);
                return;
            }

            LinkedList<MsAlarmInformationDo> msAlarmInformationDoList = new LinkedList<>();

            // 获取演示模式标识；2022-10-31 13:57:30
            Boolean isDemoMode = InitDemoMode.getIsDemoMode();
            if (Boolean.TRUE.equals(enableTableRule)) {
                userVisitedTableIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList, portraitConfig, isDemoMode);
            }
            if (Boolean.TRUE.equals(enableTimeRule)) {
                userVisitedTimeIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList, portraitConfig, isDemoMode);
            }

            // 宇翔，你看看加在这个地方，是否合适？
            highRiskOptService.visitIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);

            // 将异常告警信息发送到Kafka中；2022-10-19 10:07:34
            sendMsAlarmInformationDoToKafka(msAlarmInformationDoList);
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.doUserVisitedIsAbnormal() # 进行异常检测时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：sendMsAlarmInformationDoToKafka</B>
     * <B>概要说明：将异常告警信息发送到Kafka中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-19 10:07:44
     * @Param [msAlarmInformationDoList]
     **/
    private void sendMsAlarmInformationDoToKafka(List<MsAlarmInformationDo> msAlarmInformationDoList) {
        try {
            if (msAlarmInformationDoList.size() != 0) {
                // 将告警检测发送到Kafka中
                MsConsumerRecords msConsumerRecords = new MsConsumerRecords(RecordEnum.ANOMALY_ALARM.getCode(), msAlarmInformationDoList);
                aiitKafkaProducer.sendWithKey(anomalyDetectionAlarmTopic, Const.ANOMALY_DETECTION_ALARM_KEY, JsonUtil.obj2String(msConsumerRecords));
            }
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.sendMsAlarmInformationDoToKafka() # 将告警消息发送到Kafka中的时候，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：userPortraitInitNotDone</B>
     * <B>概要说明：当项目启动后，如果用户画像没有初始化完毕，那么将待异常检测的用户行为信息发送到Kafka中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 10:32:58
     * @Param []
     **/
    public Boolean userPortraitInitNotDone(List<MsSegmentDetailDo> segmentDetaiDolList) {
        // 当项目启动后，如果用户画像一直没有初始化完毕，那么将待异常检测的用户行为信息发送到Kafka中。
        if (Boolean.FALSE.equals(MsCaffeineCache.getUserPortraitInitDone()) && !segmentDetaiDolList.isEmpty()) {
            try {
                MsConsumerRecords msConsumerRecords = new MsConsumerRecords(RecordEnum.MSSEGMENTDETAILDO_CONSUME_FAILED.getCode(), segmentDetaiDolList);
                aiitKafkaProducer.send(anomalyDetectionConsumeFailedTopic, JsonUtil.obj2String(msConsumerRecords));
                return false;
            } catch (Exception e) {
                log.error("# AnomalyDetectionBusiness.userPortraitInitNotDone() # 开始执行异常检测，由于用户画像还没有初始化完毕，在这里将待异常检测的信息发送到Kakfa的topic = 【{}】时，出现了异常。", anomalyDetectionConsumeFailedTopic, e);
            }
        }
        return true;
    }

    /**
     * 获取规则开关
     */
    public Boolean getEnableRule(String suffix) {
        String key = AnomalyConst.RULE_PREFIX + suffix;
        // 从Redis中读取
        Object o = redisPoolUtil.get(key);
        if (o != null) {
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

    /**
     * <B>方法名称：getCoarseCountsOfUsers</B>
     * <B>概要说明：获取用户访问行为信息</B>
     *
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022-11-04 10:58:55
     * @Param [username, pageNo, pageSize]
     **/
    public ServerResponse<String> getCoarseCountsOfUsers(String username, Integer pageNo, Integer pageSize) {
        if (pageNo < 0) {
            return ServerResponse.createByErrorMessage("页码参数pageNo不能小于0", "");
        }

        // 用户名不为空，则是获取指定的用户的访问信息，那么直接从数据库中获取；2022-11-04 10:52:37
        if (StringUtil.isNotBlank(username)) {
            return getCoarseCountsOfUsersFromDb(username, pageNo, pageSize);
        }

        // 从有序集合zset中获取指定用户访问次数最多的表；2022-07-20 14:29:13
        Set<ZSetOperations.TypedTuple<String>> typedTuples = redisPoolUtil.reverseRangeWithScores(Const.ZSET_USER_ACCESS_BEHAVIOR, (pageNo - 1) * pageSize.longValue(), pageSize.longValue() - 1);
        if (null == typedTuples || typedTuples.isEmpty()) {
            // 从Redis中获取不到数据，则从数据库中获取；
            return getCoarseCountsOfUsersFromDb(username, pageNo, pageSize);
        }
        Instant now = Instant.now();
        List<UserCoarseInfo> coarseInfoList = new LinkedList<>();
        Iterator<ZSetOperations.TypedTuple<String>> iterator = typedTuples.iterator();
        while (iterator.hasNext()) {
            ZSetOperations.TypedTuple<String> key = iterator.next();
            Double score = key.getScore();
            String value = key.getValue();
            String[] split = value.split(Const.AND);
            String userName = split[0];
            String latestAccessTime = split[1];
            String tableName = split[2];

            UserCoarseInfo userCoarseInfo = new UserCoarseInfo();
            userCoarseInfo.setUserName(userName);
            userCoarseInfo.setLastVisitedDate(latestAccessTime);
            userCoarseInfo.setVisitedCount(score.longValue());
            // 获取表对应的中文描述信息；2022-07-21 16:55:47
            String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(tableName);
            ObjectNode jsonObject = JsonUtil.createJsonObject();
            jsonObject.put("tableName", tableName);
            jsonObject.put("tableNameDesc", tableDesc);
            userCoarseInfo.setUsualVisitedData(jsonObject.toString());
            coarseInfoList.add(userCoarseInfo);

        }
        Long allUserCount = redisPoolUtil.sizeFromZset(Const.ZSET_USER_ACCESS_BEHAVIOR);
        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", coarseInfoList);
        context.put("total", allUserCount);
        log.info("执行完毕 AnomalyDetectionBusiness.getCoarseCountsOfUsers() # 从Redis中获取用户的调用链信息，耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));

        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, JsonUtil.obj2String(context));
    }

    /**
     * <B>方法名称：getCoarseCountsOfUsersFromDb</B>
     * <B>概要说明：从数据库中获取用户访问数据</B>
     *
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022-11-04 10:39:24
     * @Param [username, pageNo, pageSize]
     **/
    private ServerResponse<String> getCoarseCountsOfUsersFromDb(String username, Integer pageNo, Integer pageSize) {
        Instant now = Instant.now();
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        Integer period = portraitConfig.getRuleTablePeriod();

        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(username)) {
            queryMap.put(Const.USER_NAME, username);
        }
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);
        queryMap.put(Const.PERIOD, period);

        List<UserCoarseInfo> coarseInfoList = new ArrayList<>();

        List<String> users = tableMapper.getAllUser(queryMap);
        for (String user : users) {
            String tableName = null;
            // 从有序集合zset中获取指定用户访问次数最多的表；2022-07-20 14:29:13
            Set<String> set = redisPoolUtil.reverseRange(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + user, 0L, 0L);
            if (null == set || set.isEmpty()) {
                // 从数据库中获取用户名；
                tableName = getTableNameFromDb(user);
                if (StringUtil.isBlank(tableName)) {
                    continue;
                }
            } else {
                Object[] objects = set.toArray();
                tableName = String.valueOf(objects[0]);
            }
            getUserCoarseInfo(coarseInfoList, user, tableName);
        }

        Integer allUserCount = tableMapper.getAllUserCount(queryMap);
        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", coarseInfoList);
        context.put("total", allUserCount);
        log.info("执行完毕 AnomalyDetectionBusiness.getCoarseCountsOfUsers() # 从数据库中获取用户的调用链信息，耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));

        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, JsonUtil.obj2String(context));
    }

    private void getUserCoarseInfo(List<UserCoarseInfo> coarseInfoList, String user, String tableName) {
        UserCoarseInfo userCoarseInfo = new UserCoarseInfo();
        userCoarseInfo.setUserName(user);
        userCoarseInfo.setLastVisitedDate((String) redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + user));
        // 根据用户名获取用户对数据库总的访问次数；
        Object obj = redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + user);
        Long countFromRedis = 0L;
        if (null != obj) {
            countFromRedis = Long.valueOf(String.valueOf(obj));
        }
        userCoarseInfo.setVisitedCount(countFromRedis);

        // 获取表对应的中文描述信息；2022-07-21 16:55:47
        String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(tableName);
        ObjectNode jsonObject = JsonUtil.createJsonObject();
        jsonObject.put("tableName", tableName);
        jsonObject.put("tableNameDesc", tableDesc);
        userCoarseInfo.setUsualVisitedData(jsonObject.toString());

        coarseInfoList.add(userCoarseInfo);
    }

    /**
     * <B>方法名称：getTableNameFromDb</B>
     * <B>概要说明：从数据库中获取用户名；</B>
     *
     * @return
     * @Author zm
     * @Date 2022年07月19日 15:07:00
     * @Param
     **/
    private String getTableNameFromDb(String userName) {
        Map<String, String> tableNameMap = msSegmentDetailDao.selectUserUsualAndUnusualDataByUserName(userName);
        try {
            if (null != tableNameMap) {
                String msTableName = tableNameMap.get(Const.MS_TABLE_NAME);
                String peer = tableNameMap.get(Const.PEER);
                String dbInstance = tableNameMap.get(Const.DB_INSTANCE2);
                return peer + Const.POUND_KEY + dbInstance + Const.POUND_KEY + msTableName;
            }
        } catch (Exception e) {
            log.error("# SegmentDetailServiceImpl.getTableNameFromDb() # 从数据库中获取用户名时，出现了异常。", e);
        }
        return null;
    }


    /**
     * 钉钉告警
     */
    public void dingAlarm(List<MsAlarmInformationDo> msAlarmInformationDoList) {
        try {
            HashMap<String, Integer> map = new HashMap<>(Const.NUMBER_EIGHT);
            for (MsAlarmInformationDo msAlarmInformation : msAlarmInformationDoList) {
                DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                String strDate = dateFormat.format(msAlarmInformation.getOriginalTime());
                map.computeIfAbsent(msAlarmInformation.getMatchRuleId() +
                        Const.POUND_KEY + msAlarmInformation.getUserName() +
                        Const.POUND_KEY + strDate, k -> 1);
                map.computeIfPresent(msAlarmInformation.getMatchRuleId() +
                        Const.POUND_KEY + msAlarmInformation.getUserName() +
                        Const.POUND_KEY + strDate, (k, v) -> v + 1);
            }
            for (Map.Entry<String, Integer> entry : map.entrySet()) {
                dingAlarmHelper(entry.getKey(), entry.getValue());
            }
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.dingAlarm() # 钉钉告警信息转换出现异常。", e);
        }
    }

    /**
     * 告警单条消息
     */
    private void dingAlarmHelper(String key, Integer count) {
        try {
            log.info("开始钉钉告警信息存储");
            MsCaffeineCache.setDingInfoInsertedDone(false);
            String[] strings = key.split(Const.POUND_KEY);
            Date date = null;
            try {
                date = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(strings[2]);
            } catch (ParseException e) {
                log.error("提取时间失败----{}", strings[2]);
                return;
            }
            //插入钉钉信息表
            dingAlarmInformationMapper.insert(DingAlarmInformation.builder()
                    .username(strings[1])
                    .createTime(date)
                    .triggerTimes(count)
                    .ruleId(Integer.valueOf(strings[0]))
                    .isDelete(Const.IS_DELETE_ZERO)
                    .build());
            MsCaffeineCache.setDingInfoInsertedDone(true);
            log.info("钉钉告警信息存储成功");
        } catch (Exception e) {
            log.error("钉钉告警存储发生异常:{}", e.getMessage());
        }
    }

    /**
     * 从Redis中获取画像信息
     */
    public Boolean getPortraitFromRedis() {
        try {
            // 库表画像
            Map<Object, Object> map = redisPoolUtil.hmget(AnomalyConst.REDIS_TABLE_PORTRAIT_PREFIX);
            Map<String, String> strMap = map.entrySet().stream().collect(Collectors.toMap(e -> String.valueOf(e.getKey()), e -> String.valueOf(e.getValue())));
            if (null != strMap && !strMap.isEmpty()) {
                if (MsCaffeineCache.getUserPortraitByTableLocalCacheIsReady()) {
                    MsCaffeineCache.putAllIntoPortraitByTableLocalCache(strMap);
                } else {
                    log.error("# AnomalyDetectionBusiness.getPortraitFromRedis() # 从Redis中获取画像信息时，由于本地缓存userPortraitByTableLocalCache还没创建好，所以无法把Redis中的画像同步到本地。");
                }
            }
            // 时间小时时段画像
            map = redisPoolUtil.hmget(AnomalyConst.REDIS_TIME_PORTRAIT_PREFIX);
            strMap = map.entrySet().stream().collect(Collectors.toMap(e -> String.valueOf(e.getKey()), e -> String.valueOf(e.getValue())));
            if (null != strMap) {
                if (MsCaffeineCache.getUserPortraitByTimeLocalCacheIsReady()) {
                    MsCaffeineCache.putAllIntoPortraitByTimeLocalCache(strMap);
                } else {
                    log.error("# AnomalyDetectionBusiness.getPortraitFromRedis() # 从Redis中获取画像信息时，由于本地缓存userPortraitByTimeLocalCache还没创建好，所以无法把Redis中的画像同步到本地。");
                }
            }
            // 时间早中晚分区时段画像
            map = redisPoolUtil.hmget(AnomalyConst.REDIS_TIME_PARTITION_PORTRAIT_PREFIX);
            strMap = map.entrySet().stream().collect(Collectors.toMap(e -> String.valueOf(e.getKey()), e -> String.valueOf(e.getValue())));
            if (null != strMap) {
                if (MsCaffeineCache.getUserPortraitByTimePartitionLocalCacheIsReady()) {
                    MsCaffeineCache.putAllIntoPortraitByTimePartitionLocalCache(strMap);
                } else {
                    log.error("# AnomalyDetectionBusiness.getPortraitFromRedis() # 从Redis中获取画像信息时，由于本地缓存userPortraitByTimePartitionLocalCache还没创建好，所以无法把Redis中的画像同步到本地。");
                }
            }
        } catch (Exception e) {
            log.error("# AnomalyDetectionBusiness.getPortraitFromRedis() # 从Redis中获取画像信息时，出现了异常。", e);
            return false;
        }
        return true;
    }
}
