package com.mingshi.skyflying.agent;

import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.utils.StringUtil;
import org.apache.kafka.common.utils.CopyOnWriteMap;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: AgentInformationSingleton</B>
 * <B>概要说明：存储探针信息的单例</B>
 * Author zm
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

  private AgentInformationSingleton() {
    copyOnWriteMap = new CopyOnWriteMap();
  }

  static class Singleton {
    private static AgentInformationSingleton agentInformationSingleton = new AgentInformationSingleton();
  }

  public static CopyOnWriteMap<String, String> getInstance() {
    return Singleton.agentInformationSingleton.copyOnWriteMap;
  }

  public static String get(String key) {
    return getInstance().get(key);
  }

  public static void putIfAbsent(String key, String value) {
    if(StringUtil.isBlank(key) || StringUtil.isBlank(value)){
      return;
    }
    String ifAbsent = getInstance().putIfAbsent(key, value);
    if(StringUtil.isBlank(ifAbsent) || ifAbsent.equals(Const.DOLLAR)){
      atomicBoolean.set(true);
    }
  }

}
