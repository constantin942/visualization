package com.mingshi.skyflying.init;

import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTable;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTableMapper;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTimeMapper;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTableDo;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.enums.ConstantsCode;
import com.mingshi.skyflying.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
  private UserPortraitByVisitedTableMapper userPortraitByVisitedTableMapper;

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
    initUserPortraitByVisitedTimeMap();
    initUserPortraitByVisitedTableMap();
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
  public Boolean initUserPortraitByVisitedTableMap() {
    Map<String, Map<String, Integer>> userPortraitByVisitedTableMap = AnomylyDetectionSingletonByVisitedTable.getUserPortraitByVisitedTableMap();
    Instant now = Instant.now();
    List<UserPortraitByVisitedTableDo> list = userPortraitByVisitedTableMapper.selectAll();
    if (null == list || 0 == list.size()) {
      if (0 == atomicInteger.incrementAndGet() % 500) {
        log.error("# LoadUserPortraitFromDb.initUserPortraitByVisitedTableMap() # 从数据库中没有查询到用户的访问过的表的画像信息。");
      }
      return false;
    }
    for (UserPortraitByVisitedTableDo userPortraitByVisitedTableDo : list) {
      String tableName = userPortraitByVisitedTableDo.getVisitedTable();
      Integer visitedCount = userPortraitByVisitedTableDo.getVisitedCount();

      String userName = userPortraitByVisitedTableDo.getUserName();
      Map<String, Integer> map = userPortraitByVisitedTableMap.get(userName);
      if (null == map) {
        map = new HashMap<>();
        userPortraitByVisitedTableMap.put(userName, map);
      }
      if (null != visitedCount) {
        map.put(tableName, visitedCount);
      }
    }
    log.info("# LoadUserPortraitFromDb.initUserPortraitByVisitedTableMap() # 项目启动完毕，从数据库中查询到用户访问过的表的画像信息【{}条】，用时【{}】毫秒。", userPortraitByVisitedTableMap.size(), DateTimeUtil.getTimeMillis(now));
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
    Map<String, Map<String, Integer>> userPortraitByVisitedTimeMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
    Instant now = Instant.now();
    List<UserPortraitByVisitedTimeDo> list = userPortraitByVisitedTimeMapper.selectAll();
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
        map.put(ConstantsCode.USER_PORTRAIT_FORENOON.getMsgEn(), forenoonCount);
      }
      if (null != afternoonCount) {
        map.put(ConstantsCode.USER_PORTRAIT_AFTERNOON.getMsgEn(), afternoonCount);
      }
      if (null != nightCount) {
        map.put(ConstantsCode.USER_PORTRAIT_NIGHT.getMsgEn(), nightCount);
      }
    }
    log.info("# LoadUserPortraitFromDb.initUserPortraitByVisitedTimeMap() # 项目启动完毕，从数据库中查询到用户的基于访问时间的画像信息【{}条】，用时【{}】毫秒。", userPortraitByVisitedTimeMap.size(), DateTimeUtil.getTimeMillis(now));
    return true;
  }
}
