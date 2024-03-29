package com.aiit.skyflying.anomaly_detection.task;

import com.aiit.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.aiit.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.aiit.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.aiit.skyflying.anomaly_detection.dao.UserPortraitByTableMapper;
import com.aiit.skyflying.anomaly_detection.domain.PortraitConfig;
import com.aiit.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.aiit.skyflying.common.constant.AnomalyConst;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.dao.MsSegmentDetailDao;
import com.aiit.skyflying.common.domain.MsAlarmInformationDo;
import com.aiit.skyflying.common.domain.MsSegmentDetailDo;
import com.aiit.skyflying.common.utils.RedisPoolUtil;
import com.aiit.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.Resource;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * @Author: 唐郑翔
 * @Description: 基于库表的访问规则画像表即粗粒度表
 * @Date: create in 2022/9/7
 */
@Slf4j
@Configuration
@EnableScheduling
public class UserPortraitByTableTask {

    @Resource
    RedissonClient redissonClient;

    @Resource
    UserPortraitByTableMapper userPortraitByTableMapper;

    @Resource
    private MsSegmentDetailMapper segmentDetailMapper;

    @Resource
    private PortraitConfigMapper portraitConfigMapper;
    @Resource
    private MsSegmentDetailDao msSegmentDetailDao;

    @Resource
    RedisPoolUtil redisPoolUtil;

    @Resource
    UserPortraitByTableMapper tableMapper;

    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:insInfo2PortraitOnTable";

    /**
     * 用户访问行为key
     */
    public static final String REDIS_LOCK_USER_ACCESS_BEHAVIOR = "anomaly_detection:userAccessBehavior";

    /**
     * 每日定时任务 : 全量表生成用户画像 -> 放入Redis
     */
    @Scheduled(cron = "0 0 1 * * ?")
    @Async
    public void createUserPortraitTask() {
        RLock lock = redissonClient.getLock(REDIS_LOCK);
        boolean tryLock = Boolean.FALSE;
        try {
            // 应该使用tryLock，而不是使用lock。使用lock，每个客户端阻塞等待执行定时任务。tryLock锁是只有加锁成功的客户端才能执行定时任务，其他获取锁失败的客户端，则不用执行定时任务。2022-11-04 09:29:21
            tryLock = lock.tryLock();
            if (Boolean.FALSE.equals(tryLock)) {
                return;
            }
            log.info("开始执行基于库表画像定时任务: 全量表生成用户画像 -> 放入Redis");
            //1. 全量表生成用户画像
            insertYesterdayInfo2Portrait();
            //2. 放入Redis
            cachePortraitByTable();
        } catch (Exception e) {
            log.error("生成用户画像异常 # 异常信息:{}", e.getMessage());
        } finally {
            if (Boolean.TRUE.equals(tryLock)) {
                lock.unlock();
                log.info("基于库表画像定时任务完成");
            } else {
                log.info("开始执行基于库表画像定时任务: 全量表生成用户画像 -> 放入Redis，当前实例没有获取到分布式锁。");
            }
        }
    }

    /**
     * <B>方法名称：createUserAccessBehavior</B>
     * <B>概要说明：生成用户访问行为信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-04 09:35:44
     * @Param []
     **/
    @Scheduled(cron = "0/30 * * * * ? ")
    @Async
    public void createUserAccessBehavior() {
        RLock lock = redissonClient.getLock(REDIS_LOCK_USER_ACCESS_BEHAVIOR);
        boolean tryLock = Boolean.FALSE;
        try {
            tryLock = lock.tryLock();
            if (Boolean.FALSE.equals(tryLock)) {
                return;
            }
            log.info("# UserPortraitByTableTask.createUserAccessBehavior() # 开始执行生成用户访问行为信息定时任务");
            PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
            Integer period = portraitConfig.getRuleTablePeriod();

            Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
            queryMap.put(Const.PERIOD, period);

            Set<ZSetOperations.TypedTuple<String>> typedTupleSet = new HashSet<>();
            List<String> users = tableMapper.getAllUser(queryMap);
            for (String user : users) {
                String lastVisitedDate = (String) redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + user);
                // 根据用户名获取用户对数据库总的访问次数；
                Object obj = redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + user);
                Double countFromRedis = 0d;
                if (null != obj) {
                    countFromRedis = Double.valueOf(String.valueOf(obj));
                }
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

                String value = user + Const.AND + lastVisitedDate + Const.AND + tableName;

                double score = null == countFromRedis ? 0 : countFromRedis;
                DefaultTypedTuple member = new DefaultTypedTuple(value, score);
                typedTupleSet.add(member);
            }
            if (!typedTupleSet.isEmpty()) {
                redisPoolUtil.zAddBatch(Const.ZSET_USER_ACCESS_BEHAVIOR, typedTupleSet);
                // 设置30秒的有效期；2022-11-04 10:26:18
                redisPoolUtil.expire(Const.ZSET_USER_ACCESS_BEHAVIOR, Const.NUMBER_THIRTY);
            }
        } catch (Exception e) {
            log.error("# UserPortraitByTableTask.createUserAccessBehavior() # 执行生成用户访问行为信息定时任务出现异常。", e);
        } finally {
            if (Boolean.TRUE.equals(tryLock)) {
                lock.unlock();
                log.info("# UserPortraitByTableTask.createUserAccessBehavior() # 执行生成用户访问行为信息定时任务完成");
            } else {
                log.info("# UserPortraitByTableTask.createUserAccessBehavior() # 执行生成用户访问行为信息定时任务，当前实例没有获取到分布式锁。");
            }
        }
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
     * 放入Redis
     * key : PREFIX + username + 表名
     * value : 对应表的访问次数
     */
    public void cachePortraitByTable() {
        // 这里使用selectOne()方法，不好，因为强依赖数据库表中的id，一旦数据库表中的数据前后发生变化，这里的代码在逻辑上就会出现错误。
        // 比较好的做法：在字段中找一个唯一的标识，按照这个标识来搜索。这样就将对数据库表id的依赖，转为依赖某一条记录自身唯一标识。从而消除了表中记录前后顺序不一致带来的潜在风险。2022-12-08 13:48:36
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();

        // 这里应该增加一个判空处理 null != portraitConfig.getRuleTablePeriod()，如果这个字段为空，那至少要输出错误日志；2022-12-08 13:49:34
        List<UserPortraitByTableDo> userPortraitByTableDos = userPortraitByTableMapper.selectPeriodInfo(portraitConfig.getRuleTablePeriod());
        if (userPortraitByTableDos == null) {
            log.error("获取统计时间内用户库表画像信息失败");
            return;
        }
        HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap = new HashMap<>(Const.NUMBER_EIGHT);
        portraitByTableList2Map(userPortraitByTableDos, outerMap);
        HashMap<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
        for (Map.Entry<String, HashMap<String, Integer>> outerEntry : outerMap.entrySet()) {
            String username = outerEntry.getKey();
            for (Map.Entry<String, Integer> innerEntry : outerEntry.getValue().entrySet()) {
                map.put(buildKey(username, innerEntry.getKey()), innerEntry.getValue().toString());
            }
        }
        // 这里应该判断操作Redis是否成功，如果不成功，应该至少打印错误日志。比较好的做法是：把这行代码redisPoolUtil.hmset(AnomalyConst.REDIS_TABLE_PORTRAIT_PREFIX, map);放到一个while循环中，
        // 当发现操作Redis不成功时，间隔1秒钟再次尝试。当尝试了30次后，依然失败，那么就不再尝试了。输出错误日志。2022-12-08 13:53:42
        int count = Const.NUMBER_ZERO;
        while(count < Const.NUMBER_THIRTY) {
            try {
                Thread.sleep(1000);
                if (redisPoolUtil.hmset(AnomalyConst.REDIS_TABLE_PORTRAIT_PREFIX, map)) {
                    break;
                }
            } catch (Exception e) {
                count++;
                if (count == 30) {
                    log.error("用户库表画像存入Redis失败，出现了异常", e);
                }
            }
        }
    }

    public String buildKey(String username, String key) {
        return username + ":" + key;
    }

    /**
     * 周期内的访问数据列表转换map
     */
    private void portraitByTableList2Map(List<UserPortraitByTableDo> userPortraitByTableDos,
                                         HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap) {
        for (UserPortraitByTableDo userPortraitByTableDo : userPortraitByTableDos) {
            String username = userPortraitByTableDo.getUsername();
            HashMap<String, Integer> innerMap = outerMap.getOrDefault(username, new HashMap<>(Const.NUMBER_EIGHT));
            int count = innerMap.getOrDefault(userPortraitByTableDo.getTableName(), userPortraitByTableDo.getCount());
            innerMap.put(userPortraitByTableDo.getTableName(), Math.max(count, userPortraitByTableDo.getCount()));
            outerMap.put(username, innerMap);
        }
    }

    /**
     * 昨日全量数据插入画像表(粗粒度表)
     */
    public void insertYesterdayInfo2Portrait() {
        // 这一步应该是把今天一天中所有的消息都查询出来。这一做法不好的地方在于：如果今天一天的数据量不大，比如几十万条消息，那程序没啥问题。如果今天有接近百万行消息或者上百万行消息，
        // 那么这一次性从数据库中加载到应用程序的本地内存中，有可能会造成OOM。比较好的做法是：分批从MySQL中获取消息。比如在一个while循环中，使用分页，每次获取500消息。
        int counts = Math.toIntExact(segmentDetailMapper.selectCountAll());
        int pageNo = 0;
        int pageSize = 500;
        while (pageNo*pageSize <= counts) {
            pageNo++;
            Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
            queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
            queryMap.put(Const.PAGE_SIZE, pageSize);
            List<MsSegmentDetailDo> segmentDetails = segmentDetailMapper.getInfoForCoarseDetailByPage(queryMap);
            if (segmentDetails == null) {return;}
            segmentDetails = splitTable(segmentDetails);
            List<UserPortraitByTableDo> list = getUserPortraitByTable(segmentDetails);
            if (!list.isEmpty()) {
                try {
                    log.info("昨日全量信息表插入用户库表粗粒度表成功，插入数量{}条", userPortraitByTableMapper.insertBatch(list));
                } catch (Exception e) {
                    log.error("昨日全量信息表插入用户库表粗粒度表失败，异常：", e);
                }
            }
        }
    }

    /**
     * <B>方法名称：insertAllInfo2Portrait</B>
     * <B>概要说明：当redis中所有的数据都丢了之后，重放kafka中的数据，重新生成每天的画像信息</B>
     *
     * @Author zm
     * @Date 2022-11-16 21:29:23
     * @Param []
     * @return void
     **/
    public void insertAllInfo2Portrait() {
        List<String> allStartTimeList = segmentDetailMapper.getAllStartTime();
        if(null != allStartTimeList && !allStartTimeList.isEmpty()){
            for (int i = 0; i < allStartTimeList.size(); i++) {
                String startTime = allStartTimeList.get(i);
                List<MsSegmentDetailDo> infoForCoarseDetailByStartTime = segmentDetailMapper.getInfoForCoarseDetailByStartTime(startTime);
                infoForCoarseDetailByStartTime = splitTable(infoForCoarseDetailByStartTime);
                List<UserPortraitByTableDo> list = getUserPortraitByTable(infoForCoarseDetailByStartTime);
                if (!list.isEmpty()) {
                    userPortraitByTableMapper.insertBatch(list);
                }
            }
        }
    }

    /**
     * 拆分全量信息表中的表名
     */
    public List<MsSegmentDetailDo> splitTable(List<MsSegmentDetailDo> segmentDetails) {
        List<MsSegmentDetailDo> list = new ArrayList<>();
        for (MsSegmentDetailDo segmentDetail : segmentDetails) {
            String username = segmentDetail.getUserName();
            String dbInstance = segmentDetail.getDbInstance();
            String table = segmentDetail.getMsTableName();
            if (StringUtil.isEmpty(username) || StringUtil.isEmpty(dbInstance) || StringUtil.isEmpty(table)) {
                continue;
            }
            if (!table.contains(Const.EN_COMMA)) {
                //只有一个表, 直接添加, 不用拆分
                list.add(segmentDetail);
                continue;
            }
            String[] tableNames = segmentDetail.getMsTableName().split(Const.EN_COMMA);
            for (String tableName : tableNames) {
                MsSegmentDetailDo msSegmentDetailDo = null;
                try {
                    msSegmentDetailDo = (MsSegmentDetailDo) segmentDetail.clone();
                } catch (CloneNotSupportedException e) {
                    e.printStackTrace();
                    log.error("拆分全量信息表中的表名时深拷贝出现异常, {}", e.getMessage());
                }
                assert msSegmentDetailDo != null;
                msSegmentDetailDo.setMsTableName(tableName);
                list.add(msSegmentDetailDo);
            }
        }
        return list;
    }

    /**
     * 拆分全量信息表中的表名
     */
    public List<MsAlarmInformationDo> splitTableByAlarm(List<MsAlarmInformationDo> alarmDetails) {
        List<MsAlarmInformationDo> list = new ArrayList<>();
        for (MsAlarmInformationDo alarmDetail : alarmDetails) {
            String username = alarmDetail.getUserName();
            String dbInstance = alarmDetail.getDbInstance();
            String table = alarmDetail.getMsTableName();
            if (StringUtil.isEmpty(username) || StringUtil.isEmpty(dbInstance) || StringUtil.isEmpty(table)) {
                continue;
            }
            if (!table.contains(Const.EN_COMMA)) {
                //只有一个表, 直接添加, 不用拆分
                list.add(alarmDetail);
                continue;
            }
            String[] tableNames = alarmDetail.getMsTableName().split(Const.EN_COMMA);
            for (String tableName : tableNames) {
                MsAlarmInformationDo msAlarmInformationDo = null;
                try {
                    msAlarmInformationDo = (MsAlarmInformationDo) alarmDetail.clone();
                } catch (CloneNotSupportedException e) {
                    e.printStackTrace();
                    log.error("拆分全量信息表中的表名时深拷贝出现异常, {}", e.getMessage());
                }
                assert msAlarmInformationDo != null;
                msAlarmInformationDo.setMsTableName(tableName);
                list.add(msAlarmInformationDo);
            }
        }
        return list;
    }

    /**
     * 根据全量信息获取用户画像(粗粒度表)
     */
    private List<UserPortraitByTableDo> getUserPortraitByTable(List<MsSegmentDetailDo> segmentDetails) {
        HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap = new HashMap<>(Const.NUMBER_EIGHT);
        for (MsSegmentDetailDo segmentDetail : segmentDetails) {
            String username = segmentDetail.getUserName();
            String dbInstance = segmentDetail.getDbInstance();
            String table = segmentDetail.getMsTableName();
            String tableName = dbInstance + "." + table;
            HashMap<String, Integer> innerMap = outerMap.getOrDefault(username, new HashMap<>(Const.NUMBER_EIGHT));
            int count = innerMap.getOrDefault(tableName, 0);
            innerMap.put(tableName, ++count);
            outerMap.put(username, innerMap);
        }
        return map2PortraitByTableList(outerMap);
    }

    /**
     * map转list
     */
    private List<UserPortraitByTableDo> map2PortraitByTableList(HashMap<String, HashMap<String, Integer>> map) {
        List<UserPortraitByTableDo> list = new ArrayList<>();
        for (Map.Entry<String, HashMap<String, Integer>> entry : map.entrySet()) {
            String username = entry.getKey();
            HashMap<String, Integer> innerMap = entry.getValue();
            for (Map.Entry<String, Integer> innerEntry : innerMap.entrySet()) {
                list.add(UserPortraitByTableDo.builder()
                    .username(username)
                    .tableName(innerEntry.getKey())
                    .count(innerEntry.getValue())
                    .build());
            }
        }
        return list;
    }

    /**
     * 插入库表粗粒度表/画像表
     */
    public void insertTableCoarse(MsAlarmInformationDo alarmInformationDo) {
        List<MsAlarmInformationDo> list = new ArrayList<>();
        list.add(alarmInformationDo);
        List<MsAlarmInformationDo> alarmInformationDetails = splitTableByAlarm(list);
        for (MsAlarmInformationDo alarmInformationDetail : alarmInformationDetails) {
            insertTableCoarseHelper(alarmInformationDetail);
        }
    }

    private void insertTableCoarseHelper(MsAlarmInformationDo alarmDetail) {
        String username = alarmDetail.getUserName();
        Date time = null;
        try {
            time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(alarmDetail.getStartTime());
        } catch (ParseException e) {
            log.error("提取时间失败----{}", alarmDetail.getStartTime());
            return;
        }
        String tableName = alarmDetail.getDbInstance() + "." + alarmDetail.getMsTableName();
        UserPortraitByTableDo userPortraitByTable = userPortraitByTableMapper.selectByNameAndTime(username, time, tableName);
        if (userPortraitByTable == null) {
            // 没有该用户当天粗粒度/画像信息
            userPortraitByTableMapper.insertOne(UserPortraitByTableDo.builder()
                .username(username).tableName(tableName).count(1).createTime(time)
                .build());
        } else {
            // 有该用户当天的粗粒度/画像信息
            userPortraitByTable.setCount(userPortraitByTable.getCount() + 1);
            userPortraitByTableMapper.updateByPrimaryKeySelective(userPortraitByTable);
        }
    }

    /**
     * 更新用户画像
     */
    public void updatePortrait() {
        cachePortraitByTable();
    }


    /**
     * 获取某表访问次数
     */
    public Integer getCountByTable(String username, String tableName) {
        String key = buildKey(username, tableName);
        String counts = MsCaffeineCache.getFromPortraitByTableLocalCache(key);
        if (StringUtil.isBlank(counts)) {
            return null;
        }
        return Integer.valueOf(counts.trim());
    }
}
