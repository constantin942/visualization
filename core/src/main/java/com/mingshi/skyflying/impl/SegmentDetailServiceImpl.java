package com.mingshi.skyflying.impl;


import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.elasticsearch.utils.MingshiElasticSearchUtil;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.RedisPoolUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
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
      Long countFromMySql = 0L;
      if (null != obj) {
        countFromRedis = Long.valueOf(String.valueOf(obj));
      }else{
        countFromMySql = msSegmentDetailDao.selectCountOfOneUserByUserName(userName);
      }
      countFromMySql = msSegmentDetailDao.selectCountOfOneUserByUserName(userName);

      userCoarseInfo.setVisitedCount(countFromRedis);
      if (!countFromRedis.equals(countFromMySql)) {
        log.error("# SegmentDetailServiceImpl.getCoarseCountsOfUser() #  根据用户名在Redis中获取用户对数据的访问次数【{}】与在MySQL中获取次数【{}】不一致。", countFromRedis, countFromMySql);
      }

      // 获取用户对数据库最后的访问时间；
      String latestVisitedTime = (String) redisPoolUtil.get(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + userName);
      Date lastVisited = null;
      if (StringUtil.isBlank(latestVisitedTime)) {
        lastVisited = msSegmentDetailDao.selectLastVisitedTimeByUserName(userName);
      } else {
        lastVisited = DateTimeUtil.strToDate(latestVisitedTime);
      }
      userCoarseInfo.setLastVisitedDate(lastVisited);

      // 从有序集合zset中获取指定用户访问次数最多的表；2022-07-20 14:29:13
      Set<String> set = redisPoolUtil.reverseRange(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName, 0L, 0L);
      if (null == set || 0 == set.size()) {
        // 从数据库中获取用户名；
        String tableName = getTableNameFromDb(userName);
        if (StringUtil.isBlank(tableName)) {
          continue;
        }
        userCoarseInfo.setUsualVisitedData(tableName);
      } else {
        Object[] objects = set.toArray();
        String tableNameFromRedis = String.valueOf(objects[0]);
        userCoarseInfo.setUsualVisitedData(tableNameFromRedis);

        String tableNameFromDb = getTableNameFromDb(userName);
        if (!tableNameFromRedis.equals(tableNameFromDb)) {
          log.error("开始执行 # SegmentDetailServiceImpl.getCoarseCountsOfUser # 获取用户的访问次数，根据用户名【{}】从Redis缓存中获取到的访问量最高的表是【{}】，从MySQL数据库中获取到的访问量最高的表是【{}】。", userName, tableNameFromRedis, tableNameFromDb);
        }
      }

      userCoarseInfos.add(userCoarseInfo);
    }

    //根据用户名获取用户对数据的访问次数
    // for (int i = 0; i < userCoarseInfos.size(); i++) {
    //   String userName = userCoarseInfos.get(i).getUserName();
    //   Long count = msSegmentDetailDao.selectCountOfOneUser(userName);
    //   userCoarseInfos.get(i).setVisitedCount(count);
    // }

    // 根据用户名获取用户的最后访问时间
    // for (int i = 0; i < userCoarseInfos.size(); i++) {
    //   String userName = userCoarseInfos.get(i).getUserName();
    //   Map<String, Object> queryMap = new HashMap<>();
    //   queryMap.put("userName", userName);
    //   Date lastVisited = msSegmentDetailDao.selectLastVisitedTime(queryMap);
    //   userCoarseInfos.get(i).setLastVisitedDate(lastVisited);
    // }

    // for (int i = 0; i < userCoarseInfos.size(); i++) {
    //   String userName = userCoarseInfos.get(i).getUserName();
    //   Map<String, Object> queryMap = new HashMap<>();
    //   queryMap.put("userName", userName);
    //   List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectUserUsualAndUnusualData(queryMap);
    //   Collections.sort(list, new Comparator<UserUsualAndUnusualVisitedData>() {
    //     @Override
    //     public int compare(UserUsualAndUnusualVisitedData t1, UserUsualAndUnusualVisitedData t2) {
    //       if (t1.getVisitedCount() < t2.getVisitedCount()) {
    //         return 1;
    //       } else if (t1.getVisitedCount() == t2.getVisitedCount()) {
    //         return 0;
    //       } else {
    //         return -1;
    //       }
    //     }
    //   });
    //
    //   try {
    //     if(null != list && 0 < list.size()){
    //       userCoarseInfos.get(i).setUsualVisitedData(list.get(0).getVisitedData());
    //     }
    //   } catch (Exception e) {
    //     e.printStackTrace();
    //   }
    // }

    log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfUser # 获取用户的访问次数。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", userCoarseInfos);
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

    Date lastVisited = msSegmentDetailDao.selectLastVisitedTime(queryMap);

    userCoarseInfo.setVisitedCount(count);
    userCoarseInfo.setLastVisitedDate(lastVisited);

    List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectUserUsualAndUnusualData(queryMap);
    userCoarseInfo.setUsualVisitedData(list.get(0).getVisitedData());

    log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfUser # 获取用户的访问次数。");
    List<UserCoarseInfo> userCoarseInfos = new ArrayList<>();

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
      Long countFromDb = msSegmentDetailDao.selectCountsOfAllRecentSevenDays(map);
      Date date = DateTimeUtil.strToDate(value);
      String dateToStrYYYYMMDD = DateTimeUtil.DateToStrYYYYMMDD(date);
      Long countFromRedis = 0L;
      Object hget = redisPoolUtil.hget(Const.HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS, dateToStrYYYYMMDD);
      if (null != hget) {
        countFromRedis = Long.valueOf(String.valueOf(hget));
      }
      if (!countFromDb.equals(countFromRedis)) {
        log.error("# SegmentDetailServiceImpl.getCountsOfAllRecentSevenDays() # 从Redis中获取【{}】天的数据量【{}】与MySQL中获取的数据量【{}】不一致。", value, countFromRedis, countFromDb);
      }

      returnList.add(countFromDb);
    }

    log.info("执行完毕 SegmentDetailServiceImpl # getCountsOfUserUserRecentSevenDays # 获取用户的近七天访问次数。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", returnList);
  }

  @Override
  public ServerResponse<SystemOverview> getOverviewOfSystem() {
    log.info("开始执行 # SegmentDetailServiceImpl. getOverviewOfSystem # 获取用户概览数据");
    SystemOverview systemOverview = new SystemOverview();

    // 获取ms_segment_detail表中元素的数量。先从Redis中获取，如果Redis中获取不到，再从MySQL中获取；2022-07-19 09:08:55
    Long informationCountFromRedis = 0L;
    Object hget = redisPoolUtil.get(Const.STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS);
    if (null != hget) {
      informationCountFromRedis = Long.parseLong(String.valueOf(hget));
      if (null == informationCountFromRedis) {
        informationCountFromRedis = msSegmentDetailDao.selectinformationCount();
      }
    }
    Long informationCountByMyql = msSegmentDetailDao.selectinformationCount();
    if (!informationCountFromRedis.equals(informationCountByMyql)) {
      log.error("开始执行 # SegmentDetailServiceImpl.getOverviewOfSystem # 获取用户概览数据。从Redis缓存中获取总的segmentDetail数量【{}】与在MySQL中获取到的数量【{}】不一致。", informationCountFromRedis, informationCountByMyql);
    }

    systemOverview.setVisitedInformation(informationCountFromRedis);
    // 获取数据库个数。先从表ms_monitor_business_system_tables中获取，如果获取不到，再从ms_segment_detail表中获取。2022-07-19 09:09:48
    Long dbInstanceCount = msMonitorBusinessSystemTablesMapper.selectAllDbName();
    if (null == dbInstanceCount) {
      dbInstanceCount = msSegmentDetailDao.selectDbInstanceCount();
    }
    systemOverview.setDbInstance(dbInstanceCount);

    // 获取数据库表个数。先从表ms_monitor_business_system_tables中获取，如果获取不到，再从ms_segment_detail表中获取。2022-07-19 09:09:48
    Long tableCount = msMonitorBusinessSystemTablesMapper.selectAllTables();
    if (null == tableCount) {
      tableCount = msSegmentDetailDao.selectTableCount();
    }
    systemOverview.setTable(tableCount);

    // 获取用户人数。先从表Redis中获取，如果获取不到，再从ms_segment_detail表中获取。2022-07-19 09:09:48
    Long userCountFromRedis = redisPoolUtil.setSize(Const.SET_DATA_STATISTICS_HOW_MANY_USERS);
    if (null == userCountFromRedis || 0 == userCountFromRedis) {
      userCountFromRedis = msSegmentDetailDao.selectUserCount();
    }
    Long userCountByMsql = msSegmentDetailDao.selectUserCount();
    if (!userCountFromRedis.equals(userCountByMsql)) {
      log.error("开始执行 # SegmentDetailServiceImpl. getOverviewOfSystem # 获取用户概览数据。从Redis缓存中获取总的用户人数数量【{}】与在MySQL中获取到的数量【{}】不一致。", userCountFromRedis, userCountByMsql);
    }
    systemOverview.setUser(userCountFromRedis);

    log.info("执行完毕 # SegmentDetailServiceImpl. getOverviewOfSystem # 获取用户概览数据");
    return ServerResponse.createBySuccess("获取数据成功！", "success", systemOverview);
  }

  @Override
  public ServerResponse<List<TableCoarseInfo>> getCoarseCountsOfTableName(Integer pageNo, Integer pageSize) {
    Instant now = Instant.now();
    log.info("开始执行 # SegmentDetailServiceImpl.getCoarseCountsOfTableName # 获取对于数据库的粗粒度信息。");

    List<TableCoarseInfo> tableCoarseInfos = new ArrayList<>();

    Map<String, Object> queryMap = new HashMap<>();
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);

    //获取所有的数据库表名
    List<MsMonitorBusinessSystemTablesDo> msMonitorBusinessSystemTablesDoList = msMonitorBusinessSystemTablesMapper.selectAllEnable(queryMap);
    Integer countTp = 0;
    for (MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo : msMonitorBusinessSystemTablesDoList) {
      Instant nowTp = Instant.now();
      TableCoarseInfo tableCoarseInfo = new TableCoarseInfo();
      String tableName = msMonitorBusinessSystemTablesDo.getTableName();
      tableCoarseInfo.setTableName(tableName);

      //根据数据库表获取用户对数据的访问次数
      Long count = msSegmentDetailDao.selectCountOfOneTable(tableName);
      tableCoarseInfo.setVisitedCount(count);

      // 根据用户名获取用户的最后访问时间
      Date lastVisited = msSegmentDetailDao.selectTableLastVisitedTime(tableName);
      tableCoarseInfo.setLastVisitedDate(lastVisited);

      // 获取所有访问这个表的用户及其对应的访问次数；
      List<UserUsualAndUnusualVisitedData> list = msSegmentDetailDao.selectTableUsualAndUnusualData(tableName);
      if (list.size() != 0) {
        tableCoarseInfo.setUsualVisitedUser(list.get(0).getUserName());
      } else {
        tableCoarseInfo.setUsualVisitedUser("从未有人访问过这张表");
      }

      tableCoarseInfos.add(tableCoarseInfo);
      // System.out.println("第 " + (++countTp) + " 次循环用时 " + DateTimeUtil.getTimeMillis(nowTp) + " 毫秒。");
    }

    log.info("执行完毕 SegmentDetailServiceImpl # getCoarseCountsOfTableName # 获取对于数据库的粗粒度信息，用时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
    return ServerResponse.createBySuccess("获取数据成功！", "success", tableCoarseInfos);
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
              detailJson.put("dbTableName", tableName);
              // 根据表名，去数据库中查找这个表里是存储的什么数据；2022-06-22 09:32:12
              // 正常来说，应该在本地缓存里存储表的信息，每次根据表名获取表的信息时，从本地内存中直接获取即可；2022-06-22 09:36:34
              if (StringUtil.isNotBlank(tableName)) {
                MsThirdPartyTableListDo msThirdPartyTableListDo = msThirdPartyTableListMapper.selectByTableName(tableName);
                if (null != msThirdPartyTableListDo && StringUtil.isNotBlank(msThirdPartyTableListDo.getTableDesc())) {
                  detailJson.put("function", msThirdPartyTableListDo.getTableDesc());
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
