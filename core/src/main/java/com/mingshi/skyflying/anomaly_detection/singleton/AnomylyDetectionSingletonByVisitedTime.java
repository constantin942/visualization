package com.mingshi.skyflying.anomaly_detection.singleton;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: AnomylyDetectionSingleton</B>
 * <B>概要说明：基于访问时间的异常检测单例</B>
 * Author zm
 * Date 2022/6/9 15:15
 *
 * @Version 1.0
 **/
public class AnomylyDetectionSingletonByVisitedTime {
  // 规则在数据库表里的id；
  private static volatile Integer visitedTimeRuleId = null;

  public static void setVisitedTimeRuleId(Integer visitedTimeRuleId){
    visitedTimeRuleId = visitedTimeRuleId;
  }

  public static Integer getVisitedTimeRuleId(){
    return visitedTimeRuleId;
  }


  // 存放用户基于访问时间的告警规则；2022-06-07 17:08:05
  private static volatile Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> userPortraitByVisitedTimeMap = null;

  public static Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> getUserPortraitByVisitedTimeMap() {
    if(null == userPortraitByVisitedTimeMap){
      synchronized (AnomylyDetectionSingletonByVisitedTime.class){
        if(null == userPortraitByVisitedTimeMap){
          userPortraitByVisitedTimeMap = new ConcurrentHashMap<>();
        }
      }
    }
    return userPortraitByVisitedTimeMap;
  }

  // 是否已变更的标识，当定时任务开始更新到数据库时，先判断这个标识是否已更改过。不要把未变更过的记录更新到数据库中。2022-06-08 10:44:53
  private static volatile AtomicBoolean userPortraitByVisitedTimeIsChanged = new AtomicBoolean(false);

  public static  Boolean getUserPortraitByVisitedTimeIsChanged() {
    return userPortraitByVisitedTimeIsChanged.get();
  }

  // 当userPortraitByVisitedTimeMap有数据变更时，就设置为true，然后定时任务将其更新到数据库中；2022-06-08 10:47:48
  public static void setUserPortraitByVisitedTimeIsChanged(Boolean flag) {
    userPortraitByVisitedTimeIsChanged.set(flag);
  }

  // 启动标志；只有当数据库中这条规则被启用时，才使用。
  private static volatile AtomicBoolean userPortraitByVisitedTimeEnable = new AtomicBoolean(false);

  public static Boolean getUserPortraitByVisitedTimeEnable() {
    return userPortraitByVisitedTimeEnable.get();
  }

  // 设置启用标志；2022-06-23 16:07:53
  public static void setUserPortraitByVisitedTimeEnable(Boolean flag) {
    userPortraitByVisitedTimeEnable.set(flag);
  }
}
