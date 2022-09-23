package com.mingshi.skyflying.anomaly_detection.task;

import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.dao.PortraitConfigMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTableMapper;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
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
    RedisPoolUtil redisPoolUtil;

    @Value("${anomalyDetection.redisKey.portraitByTime.prefix:anomaly_detection:portraitByTable:}")
    private String PREFIX;

    private final Integer EXPIRE = 100000;
    /**
     * Redis分布式锁Key
     */
    public static final String REDIS_LOCK = "anomaly_detection:insInfo2PortraitOnTable";

    /**
     * 每日定时任务 : 全量表生成用户画像 -> 放入Redis
     */
    @Scheduled(cron = "0 0 1 * * ?")
    private void createUserPortraitTask() {
        RLock lock = redissonClient.getLock(REDIS_LOCK);
        lock.lock();
        try {
            log.info("开始执行定时任务: 全量表生成用户画像 -> 放入Redis");
            //1. 全量表生成用户画像
            insertYesterdayInfo2Portrait();
            //2. 放入Redis
            cachePortraitByTable();
        } catch (Exception e) {
            log.error("生成用户画像异常");
        } finally {
            lock.unlock();
        }
    }

    /**
     * 放入Redis
     * key : PREFIX + username + 表名
     * value : 对应表的访问次数
     */
    public void cachePortraitByTable() {
        PortraitConfig portraitConfig = portraitConfigMapper.selectOne();
        List<UserPortraitByTableDo> userPortraitByTableDos = userPortraitByTableMapper.selectPeriodInfo(portraitConfig.getRuleTablePeriod());
        HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap = new HashMap<>();
        portraitByTableList2Map(userPortraitByTableDos, outerMap);
        for (Map.Entry<String, HashMap<String, Integer>> outerEntry : outerMap.entrySet()) {
            String username = outerEntry.getKey();
            for (Map.Entry<String, Integer> innerEntry : outerEntry.getValue().entrySet()) {
                String redisKey = buildRedisKey(username, innerEntry.getKey());
                redisPoolUtil.set(redisKey, innerEntry.getValue(), EXPIRE);
            }
        }
    }

    public String buildRedisKey(String username, String key) {
        return PREFIX + username + ":" + key;
    }

    /**
     * 周期内的访问数据列表转换map
     */
    private void portraitByTableList2Map(List<UserPortraitByTableDo> userPortraitByTableDos,
                                         HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap) {
        for (UserPortraitByTableDo userPortraitByTableDo : userPortraitByTableDos) {
            String username = userPortraitByTableDo.getUsername();
            HashMap<String, Integer> innerMap = outerMap.getOrDefault(username, new HashMap<>());
            int count = innerMap.getOrDefault(userPortraitByTableDo.getTableName(), userPortraitByTableDo.getCount());
            innerMap.put(userPortraitByTableDo.getTableName(), ++count);
            outerMap.put(username, innerMap);
        }
    }

    /**
     * 昨日全量数据插入画像表(粗粒度表)
     */
    public void insertYesterdayInfo2Portrait() {
        List<MsSegmentDetailDo> segmentDetails = segmentDetailMapper.getInfoForCoarseDetail();
        segmentDetails = splitTable(segmentDetails);
        List<UserPortraitByTableDo> list = getUserPortraitByTable(segmentDetails);
        if (!list.isEmpty()) {
            userPortraitByTableMapper.insertBatch(list);
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
            if (StringUtil.isEmpty(username) || StringUtil.isEmpty(dbInstance) || StringUtil.isEmpty(table)) continue;
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
     * 根据全量信息获取用户画像(粗粒度表)
     */
    private List<UserPortraitByTableDo> getUserPortraitByTable(List<MsSegmentDetailDo> segmentDetails) {
        HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap = new HashMap<>();
        for (MsSegmentDetailDo segmentDetail : segmentDetails) {
            String username = segmentDetail.getUserName();
            String dbInstance = segmentDetail.getDbInstance();
            String table = segmentDetail.getMsTableName();
            String tableName = dbInstance + "." + table;
            HashMap<String, Integer> innerMap = outerMap.getOrDefault(username, new HashMap<>());
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
    public void insertTableCoarse(MsSegmentDetailDo segmentDetailDo) {
        List<MsSegmentDetailDo> list = new ArrayList<>();
        list.add(segmentDetailDo);
        List<MsSegmentDetailDo> segmentDetails = splitTable(list);
        for (MsSegmentDetailDo segmentDetail : segmentDetails) {
            insertTableCoarseHelper(segmentDetail);
        }
    }

    private void insertTableCoarseHelper(MsSegmentDetailDo segmentDetail) {
        String username = segmentDetail.getUserName();
        Date time = null;
        try {
            time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(segmentDetail.getStartTime());
        } catch (ParseException e) {
            log.error("提取时间失败----{}", segmentDetail.getStartTime());
            return;
        }
        String tableName = segmentDetail.getMsTableName() + "." + segmentDetail.getMsTableName();
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
}
