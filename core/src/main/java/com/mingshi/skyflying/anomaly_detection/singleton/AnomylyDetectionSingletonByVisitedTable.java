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
public class AnomylyDetectionSingletonByVisitedTable {
  // 存放用户基于访问过的表的告警规则；2022-06-08 17:08:05
  private static volatile Map<String/* 用户名 */, Map<String/* 访问过的表 */, Integer/* 访问次数 */>> userPortraitByVisitedTableMap = null;

  private static AtomicBoolean userPortraitByVisitedTableIsChanged = new AtomicBoolean(false);

  public static Boolean getUserPortraitByVisitedTableIsChanged() {
    return userPortraitByVisitedTableIsChanged.get();
  }

  public static void setUserPortraitByVisitedTableIsChanged(Boolean flag) {
    userPortraitByVisitedTableIsChanged.set(flag);
  }

  public static Map<String/* 用户名 */, Map<String/* 访问过的表 */, Integer/* 访问次数 */>> getUserPortraitByVisitedTableMap() {
    if(null == userPortraitByVisitedTableMap){
      synchronized (AnomylyDetectionSingletonByVisitedTable.class){
        if(null == userPortraitByVisitedTableMap){
          userPortraitByVisitedTableMap = new ConcurrentHashMap<>();
        }
      }
    }
    return userPortraitByVisitedTableMap;
  }
}
