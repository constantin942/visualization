package com.mingshi.skyflying.config;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: SingletonLocalStatisticsMap</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/5/20 13:39
 *
 * @Version 1.0
 **/
public class SingletonLocalStatisticsMap {

  private static AtomicBoolean atomicBooleanIsChanged = new AtomicBoolean(false);

  private static AtomicBoolean atomicBooleanIsUpdatingData = new AtomicBoolean(false);

  public static Boolean getAtomicBooleanIsUpdatingData(){
    return atomicBooleanIsUpdatingData.get();
  }

  public static void setAtomicBooleanIsUpdatingData(Boolean flag){
    atomicBooleanIsUpdatingData.set(flag);
  }

  public static Boolean getAtomicBooleanIsChanged(){
    return atomicBooleanIsChanged.get();
  }

  public static void setAtomicBooleanIsChanged(Boolean flag){
    atomicBooleanIsChanged.set(flag);
  }

  private static Map<String/* token */, String/* userName */> tokenAndUserNameMap = new ConcurrentHashMap<>();
  private static Map<String/* globalTraceId */, String/* userName */> globalTraceIdAndUserNameMap = new ConcurrentHashMap<>();
  private static Map<String/* globalTraceId */, String/* token */> globalTraceIdAndTokenMap = new ConcurrentHashMap<>();

  public static Map<String, String> getGlobalTraceIdAndTokenMapMap() {
    return globalTraceIdAndTokenMap;
  }

  public static Map<String, String> getTokenAndUserNameMap() {
    return tokenAndUserNameMap;
  }

  public static Map<String, String> getGlobalTraceIdAndUserNameMap() {
    return globalTraceIdAndUserNameMap;
  }

  /**
   * <B>方法名称：</B>
   * <B>概要说明：</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月23日 15:05:10
   * @Param []
   **/
  public static void clearMap() {
    synchronized (SingletonLocalStatisticsMap.class) {
      globalTraceIdAndTokenMap.clear();
      globalTraceIdAndUserNameMap.clear();
    }
  }

}
