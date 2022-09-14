package com.mingshi.skyflying.anomaly_detection.task;

import com.mingshi.skyflying.anomaly_detection.dao.MsSegmentDetailMapper;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitByTableMapper;
import com.mingshi.skyflying.anomaly_detection.domain.CoarseSegmentDetailOnTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTableDo;
import com.mingshi.skyflying.anomaly_detection.domain.UserPortraitByTimeDo;
import com.mingshi.skyflying.anomaly_detection.domain.VisitCountOnTimeInterval;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.protocol.types.Field;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @Author: 唐郑翔
 * @Description: 基于库表的访问规则画像表即粗粒度表
 * @Date: create in 2022/9/7
 */
@Slf4j
@Component
public class UserPortraitByTableTask {

    @Resource
    RedissonClient redissonClient;

    @Resource
    UserPortraitByTableMapper userPortraitByTableMapper;

    @Resource
    private MsSegmentDetailMapper segmentDetailMapper;

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
    private void createUserPortraitTask() {
        RLock lock = redissonClient.getLock(REDIS_LOCK);
        lock.lock();
        try {
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
        List<UserPortraitByTableDo> userPortraitByTableDos = userPortraitByTableMapper.selectPeriodInfo();
        HashMap<String/*用户名*/, HashMap<String/*库表名*/, Integer/*访问次数*/>> outerMap = new HashMap<>();
        portraitByTableList2Map(userPortraitByTableDos, outerMap);
        for (Map.Entry<String, HashMap<String, Integer>> outerEntry : outerMap.entrySet()) {
            String username = outerEntry.getKey();
            for (Map.Entry<String, Integer> innerEntry : outerEntry.getValue().entrySet()) {
                String redisKey = buildRedisKey(username, innerEntry.getKey());
                redisPoolUtil.setNx(redisKey, innerEntry.getValue(), EXPIRE);
            }
        }
    }

    private String buildRedisKey(String username, String key) {
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
        if (list.size() != 0) {
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
            if (!table.contains(",")) {
                //只有一个表, 直接添加, 不用拆分
                list.add(segmentDetail);
                continue;
            }
            String[] tableNames = segmentDetail.getMsTableName().split(",");
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
}
