package com.mingshi.skyflying.impl;


import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.agent.AgentInformationSingleton;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsMonitorBusinessSystemTablesMapper;
import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.UserPortraitRulesMapper;
import com.mingshi.skyflying.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;

/**
 * <B>方法名称：SegmentDetailServiceImpl</B>
 * <B>概要说明：获取用户访问的链条信息</B>
 *
 * @Author zm
 * @Date 2022年04月19日 17:04:57
 * @Param
 * @return
 **/
@Slf4j
@Service("segmentDetailService")
public class SegmentDetailServiceImpl implements SegmentDetailService {
    @Resource
    private RedisPoolUtil redisPoolUtil;
    @Resource
    private MsSegmentDetailDao msSegmentDetailDao;
    @Resource
    private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private UserPortraitRulesMapper userPortraitRulesMapper;
    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    @Override
    public ServerResponse<String> getAllSegmentsBySegmentRelation(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, String operationType, Integer pageNo, Integer pageSize) {
        log.info("开始执行 # SegmentDetailServiceImpl.getAllSegmentsBySegmentRelation2() # 获取用户的调用链信息。");
        Map<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(applicationUserName)) {
            map.put(Const.USER_NAME, applicationUserName);
        }
        if (StringUtil.isNotBlank(dbType)) {
            map.put(Const.DB_TYPE2, dbType);
        }
        if (StringUtil.isNotBlank(msTableName)) {
            map.put(Const.MS_TABLE_NAME, msTableName);
        }
        if (StringUtil.isNotBlank(startTime)) {
            map.put(Const.START_TIME, startTime);
        }
        if (StringUtil.isNotBlank(endTime)) {
            map.put(Const.END_TIME, endTime);
        }
        if (StringUtil.isNotBlank(dbUserName)) {
            map.put(Const.DB_USER_NAME2, dbUserName);
        }
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        map.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        map.put(Const.PAGE_SIZE, pageSize);
        Long count;

        // 从数据库中获取一次调用链中所涉及到的segment信息；2022-06-02 17:41:11
        LinkedHashMap<String/* global_trace_id */, LinkedHashMap<String/* url */, LinkedList<MsSegmentDetailDo>>> hashMap = getSegmentDetailsFromDb(map, operationType);
        if (Const.SEND_EMAIL.equals(operationType) || Const.FILE_OUTPUT.equals(operationType) || Const.OPERATION_TYPE_DING_TALK.equals(operationType)) {
            map.put("operationType", operationType);
            count = msSegmentDetailDao.selectCountAllFileOutputAndSendEmail(map);
        } else {
            count = msSegmentDetailDao.selectCountAllNew(map);
        }

        // 组装每一条调用链信息；2022-06-02 17:41:16
        String traceInfo = getEveryCallChainInfo(hashMap);

        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", traceInfo);
        context.put("total", count);
        log.info("执行完毕 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, JsonUtil.obj2String(context));
    }

    /**
     * <B>方法名称：getAllUserName</B>
     * <B>概要说明：获取用户登录系统的用户名</B>
     *
     * @return
     * @Author zm
     * @Date 2022年06月06日 15:06:00
     * @Param
     **/
    @Override
    public ServerResponse<String> getAllUserName() {
        List<String> list = msSegmentDetailDao.selectAllUserName();
        ServerResponse<String> serverResponse = ServerResponse.createBySuccess();
        serverResponse.setData(JsonUtil.obj2String(list));
        return serverResponse;
    }


    /**
     * <B>方法名称：getAllMsTableName</B>
     * <B>概要说明：获取数据库名</B>
     *
     * @return
     * @Author zm
     * @Date 2022年06月06日 15:06:00
     * @Param
     **/
    @Override
    public ServerResponse<String> getAllMsTableName() {
        List<String> list = msSegmentDetailDao.selectAllMsTableName();
        ServerResponse<String> serverResponse = ServerResponse.createBySuccess();
        serverResponse.setData(JsonUtil.obj2String(list));
        return serverResponse;
    }

    @Override
    public ServerResponse<InstanceTable> getAllInstanceAndTableName() {
        List<InstanceTable> list = msSegmentDetailDao.selectAllInstanceAndTableName();
        ServerResponse serverResponse = ServerResponse.createBySuccess();
        serverResponse.setData(JsonUtil.obj2String(list));
        return serverResponse;
    }

    @Override
    public ServerResponse<List<String>> getCountsOfUser(String msTableName) {
        List<String> list = new LinkedList<>();
        log.info("开始执行 # SegmentDetailServiceImpl.getCountsOfUser() # 获取表【{}】的操作类型次数。", msTableName);
        Map<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
        if (msTableName.contains(Const.POUND_KEY)) {
            try {
                String[] split = msTableName.split(Const.POUND_KEY);
                String peer = split[0];
                String dbInstance = split[1];
                String tableName = split[2];
                map.put(Const.PEER, peer);
                map.put(Const.DB_INSTANCE2, dbInstance);
                map.put(Const.MS_TABLE_NAME, tableName);
            } catch (Exception e) {
                log.error("# SegmentDetailServiceImpl.getCountsOfUser() # 获取表【{}】的操作类型次数时，出现了异常。", msTableName, e);
                return ServerResponse.createByErrorMessage("参数非法！传递过来的数据库名称格式应该是：数据库地址#数据库名称#表名", Const.FAILED, list);
            }
        } else {
            return ServerResponse.createByErrorMessage("参数非法！传递过来的数据库名称格式应该是：数据库地址#数据库名称#表名", Const.FAILED, list);
        }

        String key = Const.ZSET_TABLE_OPERATION_TYPE + msTableName;
        Long sizeFromZset = redisPoolUtil.sizeFromZset(key);
        // 从有序集合zset中获取对每个表操作类型统计；2022-07-22 10:01:52
        Set<ZSetOperations.TypedTuple<String>> set = redisPoolUtil.reverseRangeWithScores(key, 0L, sizeFromZset);
        if (null != set && !set.isEmpty()) {
            Iterator<ZSetOperations.TypedTuple<String>> iterator = set.iterator();
            while (iterator.hasNext()) {
                ZSetOperations.TypedTuple<String> next = iterator.next();
                Double score = next.getScore();
                String operationType = next.getValue();
                ObjectNode jsonObject = JsonUtil.createJsonObject();
                jsonObject.put(Const.DB_TYPE2, operationType);
                jsonObject.put(Const.DB_TYPE_TIMES, score);
                list.add(jsonObject.toString());
            }
        } else {
            String[] operationTypeArray = new String[]{Const.OPERATION_TYPE_SELECT, Const.OPERATION_TYPE_UPDATE, Const.OPERATION_TYPE_DELETE, Const.OPERATION_TYPE_INSERT};
            for (String ot : operationTypeArray) {
                map.put(Const.DB_TYPE2, ot);
                Long count = msSegmentDetailDao.selectCountsOfUser(map);
                if (null != count && 0L < count) {
                    ObjectNode jsonObject = JsonUtil.createJsonObject();
                    jsonObject.put(Const.DB_TYPE2, ot);
                    jsonObject.put(Const.DB_TYPE_TIMES, count);
                    list.add(jsonObject.toString());
                }
            }
        }
        log.info("执行完毕 # SegmentDetailServiceImpl.getCountsOfUser() # 获取表【{}】的操作类型次数。", msTableName);
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, list);
    }

    /**
     * <B>方法名称：getUserOperationTypeCount</B>
     * <B>概要说明：获取用户操作类型次数</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.util.List < java.lang.String>>
     * @Author zm
     * @Date 2022年07月22日 17:07:46
     * @Param [userName]
     **/
    @Override
    public ServerResponse<List<String>> getUserOperationTypeCount(String userName) {
        List<String> list = new LinkedList<>();
        Map<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
        String key = Const.ZSET_USER_OPERATION_TYPE + userName;
        Long sizeFromZset = redisPoolUtil.sizeFromZset(key);
        // 从有序集合zset中获取对每个表操作类型统计；2022-07-22 10:01:52
        Set<ZSetOperations.TypedTuple<String>> set = redisPoolUtil.reverseRangeWithScores(key, 0L, sizeFromZset);
        if (null != set && !set.isEmpty()) {
            Iterator<ZSetOperations.TypedTuple<String>> iterator = set.iterator();
            while (iterator.hasNext()) {
                ZSetOperations.TypedTuple<String> next = iterator.next();
                Double score = next.getScore();
                String operationType = next.getValue();
                ObjectNode jsonObject = JsonUtil.createJsonObject();
                jsonObject.put(Const.DB_TYPE2, operationType);
                jsonObject.put(Const.DB_TYPE_TIMES, score);
                list.add(jsonObject.toString());
            }
        } else {
            String[] operationTypeArray = new String[]{"select", "update", "delete", "insert"};
            map.put(Const.USER_NAME, userName);
            for (String ot : operationTypeArray) {
                map.put(Const.DB_TYPE2, ot);
                Long count = msSegmentDetailDao.selectCountsOfUser(map);
                if (null != count && 0L < count) {
                    ObjectNode jsonObject = JsonUtil.createJsonObject();
                    jsonObject.put(Const.DB_TYPE2, ot);
                    jsonObject.put(Const.DB_TYPE_TIMES, count);
                    list.add(jsonObject.toString());
                }
            }
        }

        return ServerResponse.createBySuccess(list);
    }

    @Override
    public ServerResponse<String> getCoarseCountsOfUser() {
        log.info("开始执行 # SegmentDetailServiceImpl.getCoarseCountsOfUser # 获取用户的访问次数。");

        List<UserCoarseInfo> userCoarseInfos = new ArrayList<>();

        List<String> userNames = new LinkedList<>();
        //获取所有的用户名。先从Redis中获取，如果Redis中不存在，那么就从表ms_segment_detail中获取。2022-07-19 10:36:19
        Set<String> smembers = redisPoolUtil.smembers(Const.SET_DATA_STATISTICS_HOW_MANY_USERS);
        if (null == smembers || 0 == smembers.size()) {
            userNames = msSegmentDetailDao.selectAllUserName();
        } else {
            userNames.addAll(smembers);
        }
        for (int i = 0; i < userNames.size(); i++) {
            UserCoarseInfo userCoarseInfo = new UserCoarseInfo();
            String userName = userNames.get(i);
            userCoarseInfo.setUserName(userName);

            // 根据用户名获取用户对数据库总的访问次数；
            Object obj = redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + userName);
            Long countFromRedis;
            if (null != obj) {
                countFromRedis = Long.valueOf(String.valueOf(obj));
            } else {
                countFromRedis = msSegmentDetailDao.selectCountOfOneUserByUserName(userName);
            }
            userCoarseInfo.setVisitedCount(countFromRedis);

            // 获取用户对数据库最后的访问时间；
            String latestVisitedTime = (String) redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + userName);
            if (StringUtil.isBlank(latestVisitedTime)) {
                latestVisitedTime = msSegmentDetailDao.selectLastVisitedTimeByUserName(userName);
            }
            userCoarseInfo.setLastVisitedDate(latestVisitedTime);
            String tableName = null;
            // 从有序集合zset中获取指定用户访问次数最多的表；2022-07-20 14:29:13
            Set<String> set = redisPoolUtil.reverseRange(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName, 0L, 0L);
            if (null == set || set.isEmpty()) {
                // 从数据库中获取用户名；
                tableName = getTableNameFromDb(userName);
                if (StringUtil.isBlank(tableName)) {
                    continue;
                }
                // 获取表对应的中文描述信息；2022-07-21 16:55:47
                String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(tableName);
                doUsualVisitedData(tableName, userCoarseInfo, tableDesc);
            } else {
                Object[] objects = set.toArray();
                tableName = String.valueOf(objects[0]);
                // 获取表对应的中文描述信息；2022-07-21 16:55:47
                String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(tableName);
                if (StringUtil.isNotBlank(tableDesc)) {
                    tableDesc = tableDesc.trim().replace("\t", "");
                }
                doUsualVisitedData(tableName, userCoarseInfo, tableDesc);
            }
            userCoarseInfos.add(userCoarseInfo);
        }

        log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfUser # 获取用户的访问次数。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, JsonUtil.obj2String(userCoarseInfos));
    }

    /**
     * <B>方法名称：doUsualVisitedData</B>
     * <B>概要说明：给实例UserCoarseInfo赋值</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月21日 17:07:27
     * @Param [tableName, userCoarseInfo, tableDesc]
     **/
    private void doUsualVisitedData(String tableName, UserCoarseInfo userCoarseInfo, String tableDesc) {
        ObjectNode jsonObject = JsonUtil.createJsonObject();
        jsonObject.put(Const.TABLE_NAME, tableName);
        jsonObject.put(Const.TABLE_NAME_DESC, StringUtil.isBlank(tableDesc) == true ? tableName : tableDesc);
        userCoarseInfo.setUsualVisitedData(jsonObject.toString());
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

    @Override
    public ServerResponse<UserCoarseInfo> getCoarseCountsOfOneUser(String applicationUserName, Integer pageNo, Integer pageSize) {
        log.info("开始执行 # SegmentDetailServiceImpl.getCoarseCountsOfUser # 获取用户的访问次数。");

        UserCoarseInfo userCoarseInfo = new UserCoarseInfo();
        userCoarseInfo.setUserName(applicationUserName);
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(applicationUserName)) {
            queryMap.put(Const.USER_NAME, applicationUserName);
        }

        Long count = msSegmentDetailDao.selectCountOfOneUser(queryMap);

        String lastVisited = msSegmentDetailDao.selectLastVisitedTime(queryMap);
        userCoarseInfo.setVisitedCount(count);
        userCoarseInfo.setLastVisitedDate(lastVisited);

        List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectUserUsualAndUnusualData(queryMap);
        if (!list.isEmpty()) {
            userCoarseInfo.setUsualVisitedData(list.get(0).getVisitedData());
        }
        log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfUser # 获取用户的访问次数。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, userCoarseInfo);
    }

    @Override
    public ServerResponse<List<Long>> getCountsOfUserUserRecentSevenDays(String msTableName, String startTime, String endTime, Integer pageNo, Integer pageSize) {
        List<Long> returnList = new ArrayList<>();
        log.info("开始执行 # SegmentDetailServiceImpl.getCountsOfUserUserRecentSevenDays # 获取用户近七天的访问次数。");
        Map<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
        try {
            String[] split = msTableName.split("\\.");
            String dbInstance = split[0];
            String tableName = split[1];
            map.put(Const.DB_INSTANCE2, dbInstance);
            map.put(Const.MS_TABLE_NAME, tableName);
        } catch (Exception e) {
            log.error("# SegmentDetailServiceImpl.getCountsOfUserUserRecentSevenDays() # 获取用户近七天的访问次数时，出现了异常。", e);
            return ServerResponse.createByErrorMessage("参数非法", Const.FAILED, returnList);
        }

        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        map.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        map.put(Const.PAGE_SIZE, pageSize);

        List<String> dateList = DateTimeUtil.getDateList(startTime, endTime);
        for (int i = 0; i < dateList.size() - 1; i++) {
            String value = dateList.get(i);
            map.put(Const.START_TIME, value);
            map.put(Const.END_TIME, dateList.get(i + 1));
            Date date = DateTimeUtil.strToDate(value);
            String dateToStrYyyyMmDd = DateTimeUtil.dateToStrYyyyMmDd(date);
            Long count;
            Object hget = redisPoolUtil.hget(Const.HASH_TABLE_EVERYDAY_VISITED_TIMES + msTableName, dateToStrYyyyMmDd);
            if (null != hget) {
                count = Long.valueOf(String.valueOf(hget));
            } else {
                count = msSegmentDetailDao.selectCountsOfUserByPeerAndDbInstanceAndTableName(map);
            }
            returnList.add(count);
        }

        log.info("执行完毕 SegmentDetailServiceImpl # getCountsOfUserUserRecentSevenDays # 获取用户的近七天访问次数。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, returnList);
    }

    @Override
    public ServerResponse<Map<String, List<UserUsualAndUnusualVisitedData>>> getUserUsualAndUnusualData(String userName) {
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        queryMap.put(Const.USER_NAME, userName);
        HashMap<String, List<UserUsualAndUnusualVisitedData>> resMap = new HashMap<>();
        List<UserUsualAndUnusualVisitedData> highList = anomalyDetectionBusiness.getFrequentList(userName);
        List<UserUsualAndUnusualVisitedData> lowList = anomalyDetectionBusiness.getUnFrequentList(userName);
        resMap.put("high", highList);
        resMap.put("low", lowList);
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, resMap);
    }

    @Override
    public ServerResponse<List<Long>> getCountsOfAllRecentSevenDays(String startTime, String endTime) {
        log.info("开始执行 # SegmentDetailServiceImpl. getCountsOfAllRecentSevenDays # 获取用户近七天的访问次数。");
        Map<String, Object> map = new HashMap<>(Const.NUMBER_EIGHT);
        List<Long> returnList = new ArrayList<>();
        List<String> dateList = DateTimeUtil.getDateList(startTime, endTime);
        for (int i = 0; i < dateList.size() - 1; i++) {
            String value = dateList.get(i);
            map.put(Const.START_TIME, value);
            map.put(Const.END_TIME, dateList.get(i + 1));
            Date date = DateTimeUtil.strToDate(value);
            String dateToStrYyyyMmDd = DateTimeUtil.dateToStrYyyyMmDd(date);
            Long count;
            Object hget = redisPoolUtil.hget(Const.HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS, dateToStrYyyyMmDd);
            if (null != hget) {
                count = Long.valueOf(String.valueOf(hget));
            } else {
                count = msSegmentDetailDao.selectCountsOfAllRecentSevenDays(map);
            }
            returnList.add(count);
        }

        log.info("执行完毕 SegmentDetailServiceImpl # getCountsOfUserUserRecentSevenDays # 获取用户的近七天访问次数。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, returnList);
    }

    @Override
    public ServerResponse<SystemOverview> getOverviewOfSystem() {
        log.info("开始执行 # SegmentDetailServiceImpl. getOverviewOfSystem # 获取用户概览数据");
        SystemOverview systemOverview = new SystemOverview();

        // 获取ms_segment_detail表中记录的数量。
        getRecordCount(systemOverview);
        // 获取数据库个数；
        getDbCount(systemOverview);

        // 获取数据库表个数；
        getTableCount(systemOverview);

        // 获取用户人数；
        getUserCount(systemOverview);

        log.info("执行完毕 # SegmentDetailServiceImpl. getOverviewOfSystem # 获取用户概览数据");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, systemOverview);
    }

    /**
     * <B>方法名称：getRecordCount</B>
     * <B>概要说明：获取ms_segment_detail表中记录的数量</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月21日 10:07:49
     * @Param [systemOverview]
     **/
    private void getRecordCount(SystemOverview systemOverview) {
        // 获取ms_segment_detail表中记录的数量。先从Redis中获取，如果Redis中获取不到，再从MySQL中获取；2022-07-19 09:08:55
        Long informationCount;
        Object hget = redisPoolUtil.get(Const.STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS);
        if (null != hget) {
            informationCount = Long.parseLong(String.valueOf(hget));
        } else {
            informationCount = msSegmentDetailDao.selectinformationCount();
        }
        systemOverview.setVisitedInformation(informationCount);
    }

    /**
     * <B>方法名称：getUserCount</B>
     * <B>概要说明：获取用户人数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月21日 10:07:49
     * @Param [systemOverview]
     **/
    private void getUserCount(SystemOverview systemOverview) {
        // 获取用户人数。先从表Redis中获取，如果获取不到，再从ms_segment_detail表中获取。2022-07-19 09:09:48
        Long userCountFromRedis = redisPoolUtil.setSize(Const.SET_DATA_STATISTICS_HOW_MANY_USERS);
        if (null == userCountFromRedis || 0 == userCountFromRedis) {
            userCountFromRedis = msSegmentDetailDao.selectUserCount();
        }
        systemOverview.setUser(userCountFromRedis);
    }

    /**
     * <B>方法名称：getTableCount</B>
     * <B>概要说明：获取数据库表的个数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月21日 09:07:32
     * @Param [set, systemOverview]
     **/
    private void getTableCount(SystemOverview systemOverview) {
        // 从缓存里获取数据库表的个数；2022-07-21 09:47:32
        Integer tableCount = msMonitorBusinessSystemTablesMapper.selectAllEnableTableCount();
        if (null == tableCount) {
            tableCount = 0;
        }
        systemOverview.setTable(Long.valueOf(tableCount));
    }

    /**
     * <B>方法名称：getDbCount</B>
     * <B>概要说明：获取数据库的个数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月21日 09:07:16
     * @Param [set, systemOverview]
     **/
    private void getDbCount(SystemOverview systemOverview) {
        Integer dbInstanceCount = msMonitorBusinessSystemTablesMapper.selectAllEnableDbCount();
        if (null == dbInstanceCount) {
            dbInstanceCount = 0;
        }
        systemOverview.setDbInstance(Long.valueOf(dbInstanceCount));
    }

    @Override
    public ServerResponse<String> getCoarseCountsOfTableName(String tableName, Integer pageNo, Integer pageSize) {
        Instant now = Instant.now();
        log.info("开始执行 # SegmentDetailServiceImpl.getCoarseCountsOfTableName # 获取对于数据库的粗粒度信息。");

        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(tableName)) {
            queryMap.put(Const.TABLE_NAME, tableName);
        }
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        pageNo = (pageNo - 1) * pageSize;
        queryMap.put(Const.PAGE_NO, pageNo);
        queryMap.put(Const.PAGE_SIZE, pageSize);

        List<TableCoarseInfo> tableCoarseInfoList = new ArrayList<>();
        //获取所有的数据库表名
        List<MsMonitorBusinessSystemTablesDo> msMonitorBusinessSystemTablesDoListFromMySql = msMonitorBusinessSystemTablesMapper.selectAllEnable(queryMap);

        for (MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo : msMonitorBusinessSystemTablesDoListFromMySql) {
            TableCoarseInfo tableCoarseInfo = new TableCoarseInfo();
            String msTableName = msMonitorBusinessSystemTablesDo.getTableName();
            String peer = msMonitorBusinessSystemTablesDo.getDbAddress();
            String dbName = msMonitorBusinessSystemTablesDo.getDbName();

            // 获取表对应的中文描述信息；2022-07-21 16:55:47
            String getTableName = mingshiServerUtil.doGetTableName(peer, dbName, msTableName);
            String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(getTableName);
            // 根据数据库表名获取用户对该表的访问次数
            getVisitedCountByTableName(tableCoarseInfo, peer, dbName, msTableName);
            tableCoarseInfo.setTableName(getTableName);
            tableCoarseInfo.setTableNameDesc(StringUtil.isBlank(tableDesc) == true ? msTableName : tableDesc);

            // 根据表名获取最后被访问的时间
            getLatestVisitedTimeByTableName(tableCoarseInfo, peer, dbName, msTableName);

            // 获取访问这个表次数最多的用户
            getVisitedTimesMostUserName(tableCoarseInfo, peer, dbName, msTableName);

            tableCoarseInfoList.add(tableCoarseInfo);
        }
        Integer count = msMonitorBusinessSystemTablesMapper.selectAllEnableCount(queryMap);
        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        if (null != tableCoarseInfoList && !tableCoarseInfoList.isEmpty()) {
            context.put("rows", JsonUtil.obj2String(tableCoarseInfoList));
        }
        context.put("total", count);
        bySuccess.setData(JsonUtil.obj2String(context));

        log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfTableName # 获取对于数据库的粗粒度信息，用时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
        return bySuccess;
    }

    /**
     * <B>方法名称：getVisitedTimesMostUserName</B>
     * <B>概要说明：获取访问这个表次数最多的用户</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月20日 16:07:15
     * @Param [tableCoarseInfo, peer, dbName, tableName]
     **/
    private void getVisitedTimesMostUserName(TableCoarseInfo tableCoarseInfo, String peer, String dbName, String tableName) {
        String userName = null;

        String zsetVlue = peer + Const.POUND_KEY + dbName + Const.POUND_KEY + tableName;
        Set<String> set = redisPoolUtil.reverseRange(Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue, 0L, 0L);
        if (null == set || set.isEmpty()) {
            List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectTableUsualAndUnusualData(tableName);
            if (!list.isEmpty()) {
                userName = list.get(0).getUserName();
            }
        } else {
            Iterator<String> iterator = set.iterator();
            while (iterator.hasNext()) {
                userName = iterator.next();
            }
        }

        if (StringUtil.isNotBlank(userName)) {
            if (userName.contains(Const.DOLLAR)) {
                String[] split = userName.split("\\" + Const.DOLLAR);
                String serviceCodeName = AgentInformationSingleton.get(split[0]);
                if (StringUtil.isNotBlank(serviceCodeName) && !serviceCodeName.equals(Const.DOLLAR)) {
                    userName = split[1] + "（" + serviceCodeName + "）";
                }
            }
            tableCoarseInfo.setUsualVisitedUser(userName);
        } else {
            tableCoarseInfo.setUsualVisitedUser("从未有人访问过这张表");
        }
    }

    /**
     * <B>方法名称：getLatestVisitedTimeByTableName</B>
     * <B>概要说明：根据表名获取最后被访问的时间</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月20日 16:07:40
     * @Param [tableCoarseInfo, peer, dbName, tableName]
     **/
    private void getLatestVisitedTimeByTableName(TableCoarseInfo tableCoarseInfo, String peer, String dbName, String tableName) {
        String zsetVlue = peer + Const.POUND_KEY + dbName + Const.POUND_KEY + tableName;
        Object object = redisPoolUtil.get(Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue);
        String lastVisited = null;
        if (null == object) {
            lastVisited = msSegmentDetailDao.selectTableLastVisitedTime(tableName);
        } else {
            lastVisited = String.valueOf(object);
        }
        tableCoarseInfo.setLastVisitedDate(lastVisited);
    }


    /**
     * <B>方法名称：getVisitedCount</B>
     * <B>概要说明：根据数据库表名获取用户对该表的访问次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月20日 15:07:54
     * @Param [tableCoarseInfo, peer, dbName, tableName]
     **/
    private void getVisitedCountByTableName(TableCoarseInfo tableCoarseInfo, String peer, String dbName, String tableName) {
        String zsetVlue = peer + Const.POUND_KEY + dbName + Const.POUND_KEY + tableName;
        String key = Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue;
        Long size = redisPoolUtil.sizeFromZset(key);
        Set<ZSetOperations.TypedTuple<String>> set = redisPoolUtil.reverseRangeWithScores(key, 0L, size);
        Long countFromRedis = 0L;
        if (null == set || set.isEmpty()) {
            countFromRedis = msSegmentDetailDao.selectCountOfOneTable(tableName);
        } else {
            Iterator<ZSetOperations.TypedTuple<String>> iterator = set.iterator();
            while (iterator.hasNext()) {
                ZSetOperations.TypedTuple<String> next = iterator.next();
                Double score = next.getScore();
                if (null != score) {
                    countFromRedis = countFromRedis + score.longValue();
                }
            }
        }
        tableCoarseInfo.setVisitedCount(countFromRedis);
    }

    @Override
    public ServerResponse<List<AlarmData>> getAlarmData() {
        log.info("开始执行 # SegmentDetailServiceImpl.getAlarmData # 获取告警信息。");
        List<AlarmData> list = msSegmentDetailDao.selectAlarmData();
        for (int i = 0; i < list.size(); i++) {
            UserPortraitRulesDo userPortraitRulesDo = userPortraitRulesMapper.selectByPrimaryKey(list.get(i).getMatchRuleId());
            if (null != userPortraitRulesDo && StringUtil.isNotBlank(userPortraitRulesDo.getRuleDesc())) {
                list.get(i).setAlarmName(userPortraitRulesDo.getRuleDesc());
            } else {
                list.get(i).setAlarmName("异常告警");
            }
        }
        log.info("执行完毕 SegmentDetailServiceImpl # getAlarmData # 获取告警信息。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, list);

    }

    @Override
    public ServerResponse<List<UserAlarmData>> getUserAlarmData() {
        log.info("开始执行 # SegmentDetailServiceImpl.getUserAlarmData # 获取用户告警信息。");

        List<UserAlarmData> list = msSegmentDetailDao.selectUserAlarmData();

        Collections.sort(list, new Comparator<UserAlarmData>() {
            @Override
            public int compare(UserAlarmData t1, UserAlarmData t2) {
                if (t1.getAlarmCount() < t2.getAlarmCount()) {
                    return 1;
                } else if (t1.getAlarmCount().equals(t2.getAlarmCount())) {
                    return 0;
                } else {
                    return -1;
                }
            }
        });
        log.info("执行完毕 SegmentDetailServiceImpl # getAlarmData # 获取用户告警信息。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, list);
    }

    @Override
    public List<UserCoarseInfo> getCoarseCountsOfUsers(String username) {
        return anomalyDetectionBusiness.getCoarseCountsOfUsers(username);
    }


    /**
     * <B>方法名称：getSegmentDetailsFromDb</B>
     * <B>概要说明：从数据库中获取一次调用链中所涉及到的segment信息</B>
     *
     * @return java.util.HashMap<java.lang.String, java.util.HashMap < java.lang.String, java.util.List < com.mingshi.skyflying.common.domain.MsSegmentDetailDo>>>
     * @Author zm
     * @Date 2022年06月02日 17:06:01
     * @Param [map]
     **/
    private LinkedHashMap<String, LinkedHashMap<String, LinkedList<MsSegmentDetailDo>>> getSegmentDetailsFromDb(Map<String, Object> map, String operationType) {
        LinkedHashMap<String/* global_trace_id */, LinkedHashMap<String/* url */, LinkedList<MsSegmentDetailDo>>> linkedHashMap = new LinkedHashMap<>();
        try {
            List<MsSegmentDetailDo> msSegmentDetailDoList = null;
            if (Const.SEND_EMAIL.equals(operationType) || Const.FILE_OUTPUT.equals(operationType) || Const.OPERATION_TYPE_DING_TALK.equals(operationType)) {
                map.put("operationType", operationType);
                msSegmentDetailDoList = msSegmentDetailDao.selectAllFileOutputAndSendEmail(map);
            } else {
                msSegmentDetailDoList = msSegmentDetailDao.selectAllNew(map);
            }

            log.info("# SegmentDetailServiceImpl.getSegmentDetailsFromDb() # 根据查询条件 = 【{}】，在表ms_segment_detail中获取到了【{}】条详细数据。", JsonUtil.obj2String(map), msSegmentDetailDoList.size());
            if (null != msSegmentDetailDoList && !msSegmentDetailDoList.isEmpty()) {
                for (MsSegmentDetailDo msSegmentDetailDo : msSegmentDetailDoList) {
                    String globalTraceId = msSegmentDetailDo.getGlobalTraceId();
                    LinkedHashMap<String, LinkedList<MsSegmentDetailDo>> globalTraceIdHashMap = linkedHashMap.get(globalTraceId);
                    if (null == globalTraceIdHashMap) {
                        globalTraceIdHashMap = new LinkedHashMap<>();
                        linkedHashMap.put(globalTraceId, globalTraceIdHashMap);
                    }
                    String operationName = msSegmentDetailDo.getOperationName();
                    LinkedList<MsSegmentDetailDo> msSegmentDetailDosList = globalTraceIdHashMap.get(operationName);
                    if (null == msSegmentDetailDosList) {
                        msSegmentDetailDosList = new LinkedList<>();
                        globalTraceIdHashMap.put(operationName, msSegmentDetailDosList);
                    }
                    String parentSegmentId = msSegmentDetailDo.getParentSegmentId();
                    if (StringUtil.isBlank(parentSegmentId)) {
                        // 把最顶级的segment放入list的第一个位置；2022-06-02 18:07:31
                        msSegmentDetailDosList.addFirst(msSegmentDetailDo);
                    } else {
                        msSegmentDetailDosList.add(msSegmentDetailDo);
                    }
                }
            }
        } catch (Exception e) {
            log.error("# SegmentDetailServiceImpl.getSegmentDetailsFromDb() # 从数据库中获取一次调用链中所涉及到的segment信息时，出现了异常。", e);
        }
        return linkedHashMap;
    }

    /**
     * <B>方法名称：getEveryCallChainInfo</B>
     * <B>概要说明：组装每一条调用链信息</B>
     *
     * @return java.util.List<java.lang.String>
     * @Author zm
     * @Date 2022年06月02日 17:06:31
     * @Param [hashMap]
     **/
    private String getEveryCallChainInfo(LinkedHashMap<String, LinkedHashMap<String, LinkedList<MsSegmentDetailDo>>> hashMap) {
        HashSet<ObjectNode> hashSet = new HashSet<>();
        try {
            Iterator<String> iterator1 = hashMap.keySet().iterator();
            while (iterator1.hasNext()) {
                ObjectNode everyGlobalCallInfoJson = JsonUtil.createJsonObject();
                hashSet.add(everyGlobalCallInfoJson);
                ObjectNode headerJson = JsonUtil.createJsonObject();
                ArrayNode bodyJsonArray = JsonUtil.createJsonArray();
                String globalTraceId = iterator1.next();
                // 组装每一个调用链；2022-06-02 15:03:11
                HashMap<String, LinkedList<MsSegmentDetailDo>> urlHhashMap = hashMap.get(globalTraceId);
                Iterator<String> iterator2 = urlHhashMap.keySet().iterator();
                while (iterator2.hasNext()) {
                    String url = iterator2.next();
                    List<MsSegmentDetailDo> segmentDetailDoList = urlHhashMap.get(url);
                    if (null != segmentDetailDoList && !segmentDetailDoList.isEmpty()) {
                        ObjectNode jsonObject = JsonUtil.createJsonObject();
                        ArrayNode everyBodylinkedList = JsonUtil.createJsonArray();
                        bodyJsonArray.add(jsonObject);
                        everyGlobalCallInfoJson.set(Const.BODY, bodyJsonArray);
                        MsSegmentDetailDo msSegmentDetailDoBackup = new MsSegmentDetailDo();
                        for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailDoList) {
                            msSegmentDetailDoBackup = msSegmentDetailDo;
                            String parentSegmentId = msSegmentDetailDo.getParentSegmentId();
                            if (StringUtil.isBlank(parentSegmentId) && 0 == headerJson.size()) {
                                headerJson.put(Const.USER_NAME, msSegmentDetailDo.getUserName());
                                headerJson.put(Const.OPERATION_TYPE_URL, msSegmentDetailDo.getOperationName());
                                headerJson.put(Const.REQUEST_START_TIME, msSegmentDetailDo.getStartTime());
                                everyGlobalCallInfoJson.set(Const.HEADER, headerJson);
                            }
                            jsonObject.put(Const.OPERATION_TYPE_URL, msSegmentDetailDo.getOperationName());
                            ObjectNode detailJson = JsonUtil.createJsonObject();
                            detailJson.put(Const.PEER, msSegmentDetailDo.getPeer());
                            detailJson.put(Const.SERVICE_INSTANCE_NAME, msSegmentDetailDo.getServiceInstanceName());
                            detailJson.put(Const.SERVICE_CODE, msSegmentDetailDo.getServiceCode());
                            String dbType = msSegmentDetailDo.getDbType();
                            detailJson.put(Const.DB_TYPE2, dbType);
                            detailJson.put(Const.DB_INSTANCE2, msSegmentDetailDo.getDbInstance());
                            detailJson.put(Const.DB_USER_NAME2, msSegmentDetailDo.getDbUserName());
                            detailJson.put(Const.DB_STATEMENT2, msSegmentDetailDo.getDbStatement());
                            String tableName = msSegmentDetailDo.getMsTableName();
                            String peer = msSegmentDetailDo.getPeer();
                            String dbInstance = msSegmentDetailDo.getDbInstance();

                            detailJson.put(Const.DB_TABLE_NAME, tableName);
                            // 根据表名，去数据库中查找这个表里是存储的什么数据；2022-06-22 09:32:12
                            // 正常来说，应该在本地缓存里存储表的信息，每次根据表名获取表的信息时，从本地内存中直接获取即可；2022-06-22 09:36:34
                            if (StringUtil.isNotBlank(tableName)) {
                                String getTableName = mingshiServerUtil.doGetTableName(peer, dbInstance, tableName);
                                String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(getTableName);
                                if (StringUtil.isBlank(tableDesc)) {
                                    Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
                                    queryMap.put(Const.TABLE_NAME, tableName);
                                    queryMap.put(Const.DB_NAME, dbInstance);
                                    queryMap.put(Const.DB_ADDRESS, peer);
                                    MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo = msMonitorBusinessSystemTablesMapper.selectByQueryMap(queryMap);
                                    if (null != msMonitorBusinessSystemTablesDo) {
                                        String tableDesc1 = msMonitorBusinessSystemTablesDo.getTableDesc();
                                        detailJson.put(Const.FUNCTION, StringUtil.isBlank(tableDesc1) ? tableName : tableDesc1);
                                        // 设置表的描述信息到本地内存；2022-07-21 16:52:07
                                        LoadAllEnableMonitorTablesFromDb.setTableDesc(getTableName, msMonitorBusinessSystemTablesDo.getTableDesc());
                                    }
                                } else {
                                    detailJson.put(Const.FUNCTION, tableDesc);
                                }
                            }
                            everyBodylinkedList.add(detailJson);
                        }
                        if (0 == headerJson.size()) {
                            headerJson.put(Const.USER_NAME, msSegmentDetailDoBackup.getUserName());
                            headerJson.put(Const.OPERATION_TYPE_URL, msSegmentDetailDoBackup.getOperationName());
                            headerJson.put(Const.REQUEST_START_TIME, msSegmentDetailDoBackup.getStartTime());
                            everyGlobalCallInfoJson.set(Const.HEADER, headerJson);
                        }
                        jsonObject.set(Const.SEGMENTS, everyBodylinkedList);
                    }
                }
            }
        } catch (Exception e) {
            log.error("# SegmentDetailServiceImpl.getEveryCallChainInfo() # 组装每一条调用链信息时，出现了异常。", e);
        }
        if (!hashSet.isEmpty()) {
            return hashSet.toString();
        }
        return null;
    }
}
