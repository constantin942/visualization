package com.mingshi.skyflying.init;

import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTableEverydayMapper;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTimeMapper;
import com.mingshi.skyflying.dao.UserPortraitRulesMapper;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTableEverydayDo;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.domain.UserPortraitRulesDo;
import com.mingshi.skyflying.enums.ConstantsCode;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * <B>主类名称: LoadUserPortraitFromDb</B>
 * <B>概要说明：从数据库中加载用户的画像信息</B>
 * Author zm
 * Date 2022/6/7 16:55
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class LoadUserPortraitFromDb implements ApplicationRunner {
  @Resource
  private UserPortraitByVisitedTimeMapper userPortraitByVisitedTimeMapper;
  @Resource
  private UserPortraitRulesMapper userPortraitRulesMapper;
  @Resource
  private UserPortraitByVisitedTableEverydayMapper userPortraitByVisitedTableEverydayMapper;

  private AtomicInteger atomicInteger = new AtomicInteger(0);

  /**
   * <B>方法名称：run</B>
   * <B>概要说明： 项目启动，从数据库中没有查询到用户的基于访问时间的画像信息。</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 15:06:05
   * @Param [args]
   **/
  @Override
  public void run(ApplicationArguments args) throws Exception {
    // 从规则表中加载有哪些规则在使用，然后把具体的一条条详细规则加载到内存里来；2022-06-23 15:54:50
    getAllUserPortraitRules();
  }

  private void getAllUserPortraitRules() {
    List<UserPortraitRulesDo> userPortraitRulesDoList = userPortraitRulesMapper.selectAllEnableRules();
    if (null != userPortraitRulesDoList && 0 < userPortraitRulesDoList.size()) {
      for (UserPortraitRulesDo userPortraitRulesDo : userPortraitRulesDoList) {
        String ruleName = userPortraitRulesDo.getRuleName();
        if (ruleName.equals(ConstantsCode.USER_PORTRAIT_RULE_VISITED_TIME.getCode())) {
          log.info(" # LoadUserPortraitFromDb.getAllUserPortraitRules() # 项目启动，从数据库中加载【{}】规则到内存里来。", ConstantsCode.USER_PORTRAIT_RULE_VISITED_TIME.getDesc());
          initUserPortraitByVisitedTimeMap();
        } else if (ruleName.equals(ConstantsCode.USER_PORTRAIT_RULE_VISITED_TABLE.getCode())) {
          log.info(" # LoadUserPortraitFromDb.getAllUserPortraitRules() # 项目启动，从数据库中加载【{}】规则到内存里来。", ConstantsCode.USER_PORTRAIT_RULE_VISITED_TABLE.getDesc());
          initUserPortraitByVisitedTableEverydayMap();
        }
      }
    }
  }

  /**
   * <B>方法名称：initUserPortraitByVisitedTableMap</B>
   * <B>概要说明：将用户基于访问过的表的画像信息加载到本地内存中</B>
   *
   * @return java.lang.Boolean
   * @Author zm
   * @Date 2022年06月08日 17:06:14
   * @Param []
   **/
  public Boolean initUserPortraitByVisitedTableEverydayMap() {
    AnomylyDetectionSingletonByVisitedTableEveryday.setUserPortraitByVisitedTableEnable(true);

    Map<String/* 用户名 */, Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>> userPortraitByVisitedTableEverydayMap =
      AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableMap();
    Instant now = Instant.now();
    List<UserPortraitByVisitedTableEverydayDo> list = userPortraitByVisitedTableEverydayMapper.selectAllEnable();
    if (null == list || 0 == list.size()) {
      if (0 == atomicInteger.incrementAndGet() % 500) {
        log.error("# LoadUserPortraitFromDb.initUserPortraitByVisitedTableMap() # 从数据库中没有查询到用户的访问过的表的画像信息。");
      }
      return false;
    }
    for (UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo : list) {
      String tableName = userPortraitByVisitedTableEverydayDo.getVisitedTable();
      String dbType = userPortraitByVisitedTableEverydayDo.getDbType();
      Integer visitedCount = userPortraitByVisitedTableEverydayDo.getVisitedCount();
      String visitedDate = userPortraitByVisitedTableEverydayDo.getVisitedDate();
      Date date = DateTimeUtil.strToDate(visitedDate, DateTimeUtil.DATEFORMAT_STR_001);
      String strToDateToStr = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);

      String userName = userPortraitByVisitedTableEverydayDo.getUserName();
      Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>> visitedTableDateCountMap = userPortraitByVisitedTableEverydayMap.get(userName);
      if (null == visitedTableDateCountMap) {
        visitedTableDateCountMap = new ConcurrentHashMap<>();
        userPortraitByVisitedTableEverydayMap.put(userName, visitedTableDateCountMap);
      }
      Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>> dateDbTypeCountMap = visitedTableDateCountMap.get(tableName);
      if (null == dateDbTypeCountMap) {
        dateDbTypeCountMap = new ConcurrentHashMap<>();
        visitedTableDateCountMap.put(tableName, dateDbTypeCountMap);
      }
      Map<String, Integer> dbTypeCountMap = dateDbTypeCountMap.get(strToDateToStr);
      if (null == dbTypeCountMap) {
        dbTypeCountMap = new ConcurrentHashMap<>();
        dateDbTypeCountMap.put(strToDateToStr, dbTypeCountMap);
      }
      if (null != visitedCount && StringUtil.isNotBlank(dbType)) {
        dbTypeCountMap.put(dbType, visitedCount);
      }
    }
    log.info("# LoadUserPortraitFromDb.initUserPortraitByVisitedTableMap() # 项目启动完毕，从数据库中查询到用户访问过的表的画像信息【{}条】，用时【{}】毫秒。", userPortraitByVisitedTableEverydayMap.size(), DateTimeUtil.getTimeMillis(now));
    return true;
  }

  /**
   * <B>方法名称：initUserPortraitByVisitedTableMap</B>
   * <B>概要说明：将用户基于访问时间信息加载到本地内存中</B>
   *
   * @return java.lang.Boolean
   * @Author zm
   * @Date 2022年06月07日 17:06:14
   * @Param []
   **/
  public Boolean initUserPortraitByVisitedTimeMap() {
    // 设置启用标志；2022-06-23 16:08:25
    AnomylyDetectionSingletonByVisitedTime.setUserPortraitByVisitedTimeEnable(true);
    Map<String, Map<String, Integer>> userPortraitByVisitedTimeMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
    Instant now = Instant.now();
    List<UserPortraitByVisitedTimeDo> list = userPortraitByVisitedTimeMapper.selectAllEnable();
    if (null == list || 0 == list.size()) {
      if (0 == atomicInteger.incrementAndGet() % 500) {
        log.error("# LoadUserPortraitFromDb.initUserPortraitByVisitedTimeMap() # 从数据库中没有查询到用户的基于访问时间的画像信息。");
      }
      return false;
    }
    for (UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo : list) {
      Integer forenoonCount = userPortraitByVisitedTimeDo.getForenoonCount();
      Integer afternoonCount = userPortraitByVisitedTimeDo.getAfternoonCount();
      Integer nightCount = userPortraitByVisitedTimeDo.getNightCount();

      String userName = userPortraitByVisitedTimeDo.getUserName();
      Map<String, Integer> map = userPortraitByVisitedTimeMap.get(userName);
      if (null == map) {
        map = new HashMap<>();
        userPortraitByVisitedTimeMap.put(userName, map);
      }
      if (null != forenoonCount) {
        map.put(ConstantsCode.USER_PORTRAIT_FORENOON.getCode(), forenoonCount);
      }
      if (null != afternoonCount) {
        map.put(ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode(), afternoonCount);
      }
      if (null != nightCount) {
        map.put(ConstantsCode.USER_PORTRAIT_NIGHT.getCode(), nightCount);
      }
    }
    log.info("# LoadUserPortraitFromDb.initUserPortraitByVisitedTimeMap() # 项目启动完毕，从数据库中查询到用户的基于访问时间的画像信息【{}条】，用时【{}】毫秒。", userPortraitByVisitedTimeMap.size(), DateTimeUtil.getTimeMillis(now));
    return true;
  }
}
