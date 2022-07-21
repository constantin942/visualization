package com.mingshi.skyflying.impl;


import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.elasticsearch.utils.MingshiElasticSearchUtil;
import com.mingshi.skyflying.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.text.SimpleDateFormat;
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
  private UserTokenDao userTokenDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;
  @Resource
  private RedisPoolUtil redisPoolUtil;
  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
  @Resource
  private MingshiServerUtil mingshiServerUtil;
  @Resource
  private MsThirdPartyTableListMapper msThirdPartyTableListMapper;
  @Resource
  private MingshiElasticSearchUtil mingshiElasticSearchUtil;

  @Override
  public ServerResponse<String> getAllSegmentsBySegmentRelation(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize) {
    log.info("开始执行 # SegmentDetailServiceImpl.getAllSegmentsBySegmentRelation2() # 获取用户的调用链信息。");
    Map<String, Object> map = new HashMap<>();
    if (StringUtil.isNotBlank(applicationUserName)) {
      map.put("userName", applicationUserName);
    }
    if (StringUtil.isNotBlank(dbType)) {
      map.put("dbType", dbType);
    }
    if (StringUtil.isNotBlank(msTableName)) {
      map.put("msTableName", msTableName);
    }
    if (StringUtil.isNotBlank(startTime)) {
      map.put("startTime", startTime);
    }
    if (StringUtil.isNotBlank(endTime)) {
      map.put("endTime", endTime);
    }
    if (StringUtil.isNotBlank(dbUserName)) {
      map.put("dbUserName", dbUserName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    map.put("pageNo", (pageNo - 1) * pageSize);
    map.put("pageSize", pageSize);

    // 从数据库中获取一次调用链中所涉及到的segment信息；2022-06-02 17:41:11
    LinkedHashMap<String/* global_trace_id */, LinkedHashMap<String/* url */, LinkedList<MsSegmentDetailDo>>> hashMap = getSegmentDetailsFromDb(map);

    // 组装每一条调用链信息；2022-06-02 17:41:16
    String traceInfo = getEveryCallChainInfo(hashMap);

    Long count = msSegmentDetailDao.selectCountAll(map);
    Map<String, Object> context = new HashMap<>();
    context.put("rows", traceInfo);
    context.put("total", count);
    log.info("执行完毕 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", JsonUtil.obj2String(context));
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
    ServerResponse serverResponse = ServerResponse.createBySuccess();
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
    ServerResponse serverResponse = ServerResponse.createBySuccess();
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
  public ServerResponse<MsThirdPartyTableListDo> getAllInstanceTrueName() {
    List<MsThirdPartyTableListDo> list = msSegmentDetailDao.selectAllInstanceTrueName();
    ServerResponse serverResponse = ServerResponse.createBySuccess();
    serverResponse.setData(JsonUtil.obj2String(list));
    return serverResponse;
  }

  @Override
  public ServerResponse<Long> getCountsOfUser(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize) {
    log.info("开始执行 # SegmentDetailServiceImpl.getCountsOfUser # 获取用户的访问次数。");
    Map<String, Object> map = new HashMap<>();
    if (StringUtil.isNotBlank(applicationUserName)) {
      map.put("userName", applicationUserName);
    }
    if (StringUtil.isNotBlank(dbType)) {
      map.put("dbType", dbType);
    }
    if (StringUtil.isNotBlank(msTableName)) {
      map.put("msTableName", msTableName);
    }
    if (StringUtil.isNotBlank(startTime)) {
      map.put("startTime", startTime);
    }
    if (StringUtil.isNotBlank(endTime)) {
      map.put("endTime", endTime);
    }
    if (StringUtil.isNotBlank(dbUserName)) {
      map.put("dbUserName", dbUserName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    map.put("pageNo", (pageNo - 1) * pageSize);
    map.put("pageSize", pageSize);

    Long count = msSegmentDetailDao.selectCountsOfUser(map);
    log.info("执行完毕 SegmentDetailServiceImpl # getCountsOfUser # 获取用户的访问次数。");

    return ServerResponse.createBySuccess("获取数据成功！", "success", count);
  }

  @Override
  public ServerResponse<List<UserCoarseInfo>> getCoarseCountsOfUser(Integer pageNo, Integer pageSize) {
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
      Long countFromRedis = 0L;
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
      if (null == set || 0 == set.size()) {
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
        doUsualVisitedData(tableName, userCoarseInfo, tableDesc);
      }
      userCoarseInfos.add(userCoarseInfo);
    }

    log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfUser # 获取用户的访问次数。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", userCoarseInfos);
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
    String[] split = tableName.split("#");
    ObjectNode jsonObject = JsonUtil.createJSONObject();
    jsonObject.put("dbAddress", split[0]);
    jsonObject.put("dbName", split[1]);
    jsonObject.put("tableName", StringUtil.isBlank(tableDesc) == true ? tableName : tableDesc);
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
        String msTableName = tableNameMap.get("msTableName");
        String peer = tableNameMap.get("peer");
        String dbInstance = tableNameMap.get("dbInstance");
        String tableName = peer + "#" + dbInstance + "#" + msTableName;
        return tableName;
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
    Map<String, Object> queryMap = new HashMap<>();
    queryMap.put("userName", applicationUserName);

    Long count = msSegmentDetailDao.selectCountOfOneUser(queryMap);

    String lastVisited = msSegmentDetailDao.selectLastVisitedTime(queryMap);
    userCoarseInfo.setVisitedCount(count);
    userCoarseInfo.setLastVisitedDate(lastVisited);

    List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectUserUsualAndUnusualData(queryMap);
    userCoarseInfo.setUsualVisitedData(list.get(0).getVisitedData());

    log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfUser # 获取用户的访问次数。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", userCoarseInfo);
  }

  @Override
  public ServerResponse<List<Long>> getCountsOfUserUserRecentSevenDays(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize) {

    log.info("开始执行 # SegmentDetailServiceImpl.getCountsOfUserUserRecentSevenDays # 获取用户近七天的访问次数。");
    Map<String, Object> map = new HashMap<>();
    if (StringUtil.isNotBlank(applicationUserName)) {
      map.put("userName", applicationUserName);
    }
    if (StringUtil.isNotBlank(dbType)) {
      map.put("dbType", dbType);
    }
    if (StringUtil.isNotBlank(msTableName)) {
      map.put("msTableName", msTableName);
    }
    if (StringUtil.isNotBlank(dbUserName)) {
      map.put("dbUserName", dbUserName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    map.put("pageNo", (pageNo - 1) * pageSize);
    map.put("pageSize", pageSize);

    List<String> DateList = new ArrayList();

    Calendar startTimeCalendar = Calendar.getInstance();
    Calendar endTimeCalendar = Calendar.getInstance();

    Date startTimeDate = new Date();
    Date endTimeDate = new Date();

    //创建SimpleDateFormat对象实例并定义好转换格式
    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    try {
      startTimeDate = sdf.parse(startTime);
      endTimeDate = sdf.parse(endTime);
      startTimeCalendar.setTime(startTimeDate);
      endTimeCalendar.setTime(endTimeDate);
    } catch (Exception e) {

    }

    while (startTimeCalendar.compareTo(endTimeCalendar) < 1) {
      String startTimeStr = sdf.format(startTimeCalendar.getTime());
      DateList.add(startTimeStr);
      startTimeCalendar.add(Calendar.DATE, 1);
    }

    if (startTimeCalendar.before(endTimeCalendar)) {
      String endTimeStr = sdf.format(startTimeCalendar.getTime());
      DateList.add(endTimeStr);
    }

    List<Long> returnList = new ArrayList<>();

    for (int i = 0; i < DateList.size() - 1; i++) {
      map.put("startTime", DateList.get(i));
      map.put("endTime", DateList.get(i + 1));
      Long count = msSegmentDetailDao.selectCountsOfUser(map);
      returnList.add(count);
    }


    log.info("执行完毕 SegmentDetailServiceImpl # getCountsOfUserUserRecentSevenDays # 获取用户的近七天访问次数。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", returnList);
  }

  @Override
  public ServerResponse<List<UserUsualAndUnusualVisitedData>> getUserUsualAndUnusualData(String applicationUserName) {


    Map<String, Object> queryMap = new HashMap<>();
    queryMap.put("userName", applicationUserName);
    List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectUserUsualAndUnusualData(queryMap);
    Collections.sort(list, new Comparator<UserUsualAndUnusualVisitedData>() {
      @Override
      public int compare(UserUsualAndUnusualVisitedData t1, UserUsualAndUnusualVisitedData t2) {
        if (t1.getVisitedCount() < t2.getVisitedCount()) {
          return 1;
        } else if (t1.getVisitedCount() == t2.getVisitedCount()) {
          return 0;
        } else {
          return -1;
        }
      }
    });

    return ServerResponse.createBySuccess("获取数据成功！", "success", list);
  }

  @Override
  public ServerResponse<List<Long>> getCountsOfAllRecentSevenDays(String startTime, String endTime) {

    log.info("开始执行 # SegmentDetailServiceImpl. getCountsOfAllRecentSevenDays # 获取用户近七天的访问次数。");

    Map<String, Object> map = new HashMap<>();

    List<String> DateList = DateTimeUtil.getDateList(startTime, endTime);

    List<Long> returnList = new ArrayList<>();

    for (int i = 0; i < DateList.size() - 1; i++) {
      String value = DateList.get(i);
      map.put("startTime", value);
      map.put("endTime", DateList.get(i + 1));
      Date date = DateTimeUtil.strToDate(value);
      String dateToStrYYYYMMDD = DateTimeUtil.DateToStrYYYYMMDD(date);
      Long count = 0L;
      Object hget = redisPoolUtil.hget(Const.HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS, dateToStrYYYYMMDD);
      if (null != hget) {
        count = Long.valueOf(String.valueOf(hget));
      } else {
        count = msSegmentDetailDao.selectCountsOfAllRecentSevenDays(map);
      }
      returnList.add(count);
    }

    log.info("执行完毕 SegmentDetailServiceImpl # getCountsOfUserUserRecentSevenDays # 获取用户的近七天访问次数。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", returnList);
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
    return ServerResponse.createBySuccess("获取数据成功！", "success", systemOverview);
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
    Long informationCount = 0L;
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
  public ServerResponse<String> getCoarseCountsOfTableName(Integer pageNo, Integer pageSize) {
    Instant now = Instant.now();
    log.info("开始执行 # SegmentDetailServiceImpl.getCoarseCountsOfTableName # 获取对于数据库的粗粒度信息。");

    Map<String, Object> queryMap = new HashMap<>();
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    pageNo = (pageNo - 1) * pageSize;
    queryMap.put("pageNo", pageNo);
    queryMap.put("pageSize", pageSize);

    List<TableCoarseInfo> tableCoarseInfoList = new ArrayList<>();
    //获取所有的数据库表名
    List<MsMonitorBusinessSystemTablesDo> msMonitorBusinessSystemTablesDoListFromMySQL = msMonitorBusinessSystemTablesMapper.selectAllEnable(queryMap);

    for (MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo : msMonitorBusinessSystemTablesDoListFromMySQL) {
      TableCoarseInfo tableCoarseInfo = new TableCoarseInfo();
      String tableName = msMonitorBusinessSystemTablesDo.getTableName();
      String peer = msMonitorBusinessSystemTablesDo.getDbAddress();
      String dbName = msMonitorBusinessSystemTablesDo.getDbName();

      // 获取表对应的中文描述信息；2022-07-21 16:55:47
      String getTableName = mingshiServerUtil.doGetTableName(peer, dbName, tableName);
      String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(getTableName);
      // 根据数据库表名获取用户对该表的访问次数
      getVisitedCountByTableName(tableCoarseInfo, peer, dbName, tableName);
      tableCoarseInfo.setTableName(StringUtil.isBlank(tableDesc) == true ? tableName : tableDesc);

      // 根据表名获取最后被访问的时间
      getLatestVisitedTimeByTableName(tableCoarseInfo, peer, dbName, tableName);

      // 获取访问这个表次数最多的用户
      getVisitedTimesMostUserName(tableCoarseInfo, peer, dbName, tableName);

      tableCoarseInfoList.add(tableCoarseInfo);
    }
    Integer count = msMonitorBusinessSystemTablesMapper.selectAllEnableCount(queryMap);
    Map<String, Object> context = new HashMap<>();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    if (null != tableCoarseInfoList && 0 < tableCoarseInfoList.size()) {
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

    String zsetVlue = peer + "#" + dbName + "#" + tableName;
    Set<String> set = redisPoolUtil.reverseRange(Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue, 0L, 0L);
    if (null == set || 0 == set.size()) {
      List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectTableUsualAndUnusualData(tableName);
      if (list.size() != 0) {
        userName = list.get(0).getUserName();
      }
    } else {
      Iterator<String> iterator = set.iterator();
      while (iterator.hasNext()) {
        userName = iterator.next();
      }
    }

    if (StringUtil.isNotBlank(userName)) {
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
    String zsetVlue = peer + "#" + dbName + "#" + tableName;
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
    String zsetVlue = peer + "#" + dbName + "#" + tableName;
    String key = Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue;
    Long size = redisPoolUtil.sizeFromZset(key);
    Set<ZSetOperations.TypedTuple<String>> set = redisPoolUtil.reverseRangeWithScores(key, 0L, size);
    Long countFromRedis = 0L;
    if (null == set || 0 == set.size()) {
      countFromRedis = msSegmentDetailDao.selectCountOfOneTable(tableName);
    } else {
      Iterator<ZSetOperations.TypedTuple<String>> iterator = set.iterator();
      while (iterator.hasNext()) {
        ZSetOperations.TypedTuple<String> next = iterator.next();
        Double score = next.getScore();
        countFromRedis = countFromRedis + score.longValue();
      }
    }
    tableCoarseInfo.setVisitedCount(countFromRedis);
  }

  @Override
  public ServerResponse<List<AlarmData>> getAlarmData() {

    log.info("开始执行 # SegmentDetailServiceImpl.getAlarmData # 获取告警信息。");
    List<AlarmData> list = msSegmentDetailDao.selectAlarmData();
    for (int i = 0; i < list.size(); i++) {
      if (list.get(i).getMatchRuleId() == 1) {
        list.get(i).setAlarmName("基于访问时间段的告警规则：即若某用户通常白天访问数据，则夜间为异常；");
      } else if (list.get(i).getMatchRuleId() == 2) {
        list.get(i).setAlarmName("基于访问过的表的告警规则：若某用户访问从未访问过的表时，则给出告警；");
      }
    }
    log.info("执行完毕 SegmentDetailServiceImpl # getAlarmData # 获取告警信息。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", list);

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
        } else if (t1.getAlarmCount() == t2.getAlarmCount()) {
          return 0;
        } else {
          return -1;
        }
      }
    });
    log.info("执行完毕 SegmentDetailServiceImpl # getAlarmData # 获取用户告警信息。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", list);
  }


  /**
   * <B>方法名称：getSegmentDetailsFromDb</B>
   * <B>概要说明：从数据库中获取一次调用链中所涉及到的segment信息</B>
   *
   * @return java.util.HashMap<java.lang.String, java.util.HashMap < java.lang.String, java.util.List < com.mingshi.skyflying.domain.MsSegmentDetailDo>>>
   * @Author zm
   * @Date 2022年06月02日 17:06:01
   * @Param [map]
   **/
  private LinkedHashMap<String, LinkedHashMap<String, LinkedList<MsSegmentDetailDo>>> getSegmentDetailsFromDb(Map<String, Object> map) {

    LinkedHashMap<String/* global_trace_id */, LinkedHashMap<String/* url */, LinkedList<MsSegmentDetailDo>>> linkedHashMap = new LinkedHashMap<>();
    try {
      List<MsSegmentDetailDo> msSegmentDetailDoList = msSegmentDetailDao.selectAll(map);

      // 在ES中进行搜索；2022-07-07 11:13:25
      // List<EsMsSegmentDetailDo> msSegmentDetailDoListByEs = mingshiElasticSearchUtil.termQueryByMap(map);
      log.info("# SegmentDetailServiceImpl.getSegmentDetailsFromDb() # 根据查询条件 = 【{}】，在表ms_segment_detail中获取到了【{}】条详细数据。", JsonUtil.obj2String(map), msSegmentDetailDoList.size());
      if (null != msSegmentDetailDoList && 0 < msSegmentDetailDoList.size()) {
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
        ObjectNode everyGlobalCallInfoJson = JsonUtil.createJSONObject();
        hashSet.add(everyGlobalCallInfoJson);
        ObjectNode headerJson = JsonUtil.createJSONObject();
        ArrayNode bodyJsonArray = JsonUtil.createJSONArray();
        // JSONObject bodyJson = new JSONObject();
        String globalTraceId = iterator1.next();
        // 组装每一个调用链；2022-06-02 15:03:11
        HashMap<String, LinkedList<MsSegmentDetailDo>> urlHhashMap = hashMap.get(globalTraceId);
        Iterator<String> iterator2 = urlHhashMap.keySet().iterator();
        while (iterator2.hasNext()) {
          String url = iterator2.next();
          List<MsSegmentDetailDo> segmentDetailDoList = urlHhashMap.get(url);
          if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {
            ObjectNode jsonObject = JsonUtil.createJSONObject();
            ArrayNode everyBodylinkedList = JsonUtil.createJSONArray();
            bodyJsonArray.add(jsonObject);
            everyGlobalCallInfoJson.put("body", bodyJsonArray);
            MsSegmentDetailDo msSegmentDetailDoBackup = new MsSegmentDetailDo();
            for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailDoList) {
              msSegmentDetailDoBackup = msSegmentDetailDo;
              String parentSegmentId = msSegmentDetailDo.getParentSegmentId();
              if (StringUtil.isBlank(parentSegmentId) && 0 == headerJson.size()) {
                headerJson.put("userName", msSegmentDetailDo.getUserName());
                headerJson.put("url", msSegmentDetailDo.getOperationName());
                headerJson.put("requestStartTime", msSegmentDetailDo.getStartTime());
                everyGlobalCallInfoJson.put("header", headerJson);
              }
              jsonObject.put("url", msSegmentDetailDo.getOperationName());
              ObjectNode detailJson = JsonUtil.createJSONObject();
              detailJson.put("peer", msSegmentDetailDo.getPeer());
              detailJson.put("serviceInstanceName", msSegmentDetailDo.getServiceInstanceName());
              detailJson.put("serviceCode", msSegmentDetailDo.getServiceCode());
              String dbType = msSegmentDetailDo.getDbType();
              detailJson.put("dbType", dbType);
              detailJson.put("dbInstance", msSegmentDetailDo.getDbInstance());
              detailJson.put("dbUserName", msSegmentDetailDo.getDbUserName());
              detailJson.put("dbStatement", msSegmentDetailDo.getDbStatement());
              String tableName = msSegmentDetailDo.getMsTableName();
              String peer = msSegmentDetailDo.getPeer();
              String dbInstance = msSegmentDetailDo.getDbInstance();

              detailJson.put("dbTableName", tableName);
              // 根据表名，去数据库中查找这个表里是存储的什么数据；2022-06-22 09:32:12
              // 正常来说，应该在本地缓存里存储表的信息，每次根据表名获取表的信息时，从本地内存中直接获取即可；2022-06-22 09:36:34
              if (StringUtil.isNotBlank(tableName)) {
                Map<String, Object> queryMap = new HashMap<>();
                queryMap.put("tableName", tableName);
                queryMap.put("dbName", dbInstance);
                queryMap.put("dbAddress", peer);
                String getTableName = mingshiServerUtil.doGetTableName(peer, dbInstance, tableName);
                String tableDesc = LoadAllEnableMonitorTablesFromDb.getTableDesc(getTableName);
                if (StringUtil.isBlank(tableDesc)) {
                  MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo = msMonitorBusinessSystemTablesMapper.selectByQueryMap(queryMap);
                  if (null != msMonitorBusinessSystemTablesDo && StringUtil.isNotBlank(msMonitorBusinessSystemTablesDo.getTableDesc())) {
                    detailJson.put("function", msMonitorBusinessSystemTablesDo.getTableDesc());
                    // 设置表的描述信息到本地内存；2022-07-21 16:52:07
                    LoadAllEnableMonitorTablesFromDb.setTableDesc(getTableName, msMonitorBusinessSystemTablesDo.getTableDesc());
                  }
                } else {
                  detailJson.put("function", tableDesc);
                }
              }
              everyBodylinkedList.add(detailJson);
            }
            if (0 == headerJson.size()) {
              headerJson.put("userName", msSegmentDetailDoBackup.getUserName());
              headerJson.put("url", msSegmentDetailDoBackup.getOperationName());
              headerJson.put("requestStartTime", msSegmentDetailDoBackup.getStartTime());
              everyGlobalCallInfoJson.put("header", headerJson);
            }
            jsonObject.put("segments", everyBodylinkedList);
          }
        }
      }
    } catch (Exception e) {
      log.error("# SegmentDetailServiceImpl.getEveryCallChainInfo() # 组装每一条调用链信息时，出现了异常。", e);
    }
    if (0 < hashSet.size()) {
      return hashSet.toString();
    }
    return null;
  }

  /**
   * <B>方法名称：getGlobalTraceIdList</B>
   * <B>概要说明：根据用户名，获取对应全局追踪id</B>
   *
   * @return java.util.List<java.util.Map < java.lang.String, java.lang.String>>
   * @Author zm
   * @Date 2022年05月17日 17:05:04
   * @Param [userName]
   **/
  private List<Map<String, String>> getGlobalTraceIdList(String userName, Integer pageNo, Integer pageSize) {
    List<Map<String, String>> globalTraceIdMapList = new ArrayList<>();
    Map<String, Object> queryMap = new HashMap<>();
    // 设置默认值；2022-05-18 17:27:15
    if (null == pageNo) {
      queryMap.put("pageNo", 1);
    }
    if (null == pageSize) {
      queryMap.put("pageSize", 10);
    }
    // 查询出所有的traceId；2022-04-25 09:56:29
    if (StringUtil.isNotBlank(userName)) {
      queryMap.put("userName", userName);
      List<UserTokenDo> listUserToken = userTokenDao.selectByUserName(queryMap);
      if (null != listUserToken && 0 < listUserToken.size()) {
        for (UserTokenDo userTokenDo : listUserToken) {
          String globalTraceId = userTokenDo.getGlobalTraceId();
          Map<String, Object> queryMap2 = new HashMap<>();
          queryMap2.put("globalTraceId", globalTraceId);
          SegmentRelationDo segmentRelationDo = segmentRelationDao.selectByGlobalTraceId(queryMap2);
          Map<String, String> map = new HashMap<>();
          map.put("globalTraceId", segmentRelationDo.getGlobalTraceId());
          map.put("segmentIds", segmentRelationDo.getSegmentIds());
          globalTraceIdMapList.add(map);
        }
      }
    } else {
      globalTraceIdMapList = segmentRelationDao.selectAllGlobalTraceId(queryMap);
    }
    return globalTraceIdMapList;
  }
}
