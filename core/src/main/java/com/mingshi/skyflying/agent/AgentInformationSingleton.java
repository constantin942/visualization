package com.mingshi.skyflying.agent;

import com.mingshi.skyflying.common.utils.StringUtil;
import org.apache.kafka.common.utils.CopyOnWriteMap;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: AgentInformationSingleton</B>
 * <B>概要说明：存储探针信息的单例</B>
 * @Author zm
 * Date 2022/6/28 16:52
 *
 * @Version 1.0
 **/
public class AgentInformationSingleton {
  private volatile CopyOnWriteMap<String, String> copyOnWriteMap = null;

  private static volatile AtomicBoolean atomicBoolean = new AtomicBoolean(false);

  public static AtomicBoolean getAtomicBoolean(){
    return atomicBoolean;
  }

  public static void setAtomicBooleanToFalse(){
    atomicBoolean.set(false);
  }

  public static void setAtomicBooleanToTrue(){
    atomicBoolean.set(true);
  }

  private AgentInformationSingleton() {
    copyOnWriteMap = new CopyOnWriteMap();
  }

  static class Singleton {
    private static AgentInformationSingleton agentInformationSingleton = new AgentInformationSingleton();
    private Singleton(){}
  }

  public static CopyOnWriteMap<String, String> getInstance() {
    return Singleton.agentInformationSingleton.copyOnWriteMap;
  }

  public static String get(String key) {
    return getInstance().get(key);
  }

  public static void put(String key, String value) {
    if(StringUtil.isBlank(key) || StringUtil.isBlank(value)){
      return;
    }
    getInstance().put(key, value);
    atomicBoolean.set(true);
  }

}
