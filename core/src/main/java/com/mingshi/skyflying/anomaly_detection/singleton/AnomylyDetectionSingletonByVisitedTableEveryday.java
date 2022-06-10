package com.mingshi.skyflying.anomaly_detection.singleton;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: AnomylyDetectionSingletonByVisitedTable</B>
 * <B>概要说明：基于访问过的表的异常检测单例</B>
 * Author zm
 * Date 2022/6/9 15:15
 *
 * @Version 1.0
 **/
public class AnomylyDetectionSingletonByVisitedTableEveryday {
  // 存放用户基于访问过的表的告警规则；2022-06-08 17:08:05
  private static volatile Map<String/* 用户名 */, Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Integer/* 访问次数 */>>> userPortraitByVisitedTableEverydayMap = null;
  // private static volatile Map<String/* 用户名 */, Map<String/* 访问过的表 */, Integer/* 访问次数 */>> userPortraitByVisitedTableEverydayMap = null;

  private static AtomicBoolean userPortraitByVisitedTableEverydayIsChanged = new AtomicBoolean(false);

  public static Boolean getUserPortraitByVisitedTableIsChanged() {
    return userPortraitByVisitedTableEverydayIsChanged.get();
  }

  public static void setUserPortraitByVisitedTableIsChanged(Boolean flag) {
    userPortraitByVisitedTableEverydayIsChanged.set(flag);
  }

  public static Map<String/* 用户名 */, Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Integer/* 访问次数 */>>> getUserPortraitByVisitedTableMap() {
    if (null == userPortraitByVisitedTableEverydayMap) {
      synchronized (AnomylyDetectionSingletonByVisitedTableEveryday.class) {
        if (null == userPortraitByVisitedTableEverydayMap) {
          userPortraitByVisitedTableEverydayMap = new ConcurrentHashMap<>();
        }
      }
    }
    return userPortraitByVisitedTableEverydayMap;
  }
}
