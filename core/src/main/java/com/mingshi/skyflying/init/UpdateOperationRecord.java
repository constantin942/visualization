package com.mingshi.skyflying.init;

import com.mingshi.skyflying.common.constant.Const;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <B>主类名称: UpdateOperationRecord</B>
 * <B>概要说明：页面更新操作用的类</B>
 * Author zm
 * Date 2022/9/9 11:11
 *
 * @Version 1.0
 **/
public class UpdateOperationRecord {
  private static final Map<String, Map<String, String>> updateOperationRecordMap = new ConcurrentHashMap<>();

  static {
    Map<String, String> updateMonitorTableMap = new ConcurrentHashMap<>();
    updateMonitorTableMap.put("1", "禁用表");
    updateMonitorTableMap.put("0", "启用表");
    updateOperationRecordMap.put(Const.UPDATE_MONITOR_TABLE, updateMonitorTableMap);

    Map<String, String> updateMonitorTableaDescMap = new ConcurrentHashMap<>();
    updateMonitorTableaDescMap.put(Const.TABLE_DESC, "给表（" + Const.COMMA + "）起个中文名字");
    updateOperationRecordMap.put(Const.UPDATE_MONITOR_TABLE_DESC, updateMonitorTableaDescMap);

    Map<String, String> updateSkywalkingAgentMap = new ConcurrentHashMap<>();
    updateSkywalkingAgentMap.put(Const.AGENT_NAME, "给服务（" + Const.COMMA + "）起个中文名字");
    updateOperationRecordMap.put(Const.UPDATE_SKYWALKING_AGENT, updateSkywalkingAgentMap);
  }

  public static String getValue(String key, String opreation) {
    Map<String, String> map = UpdateOperationRecord.updateOperationRecordMap.get(key);
    if (null != map && map.containsKey(opreation)) {
      return map.get(opreation);
    }
    return null;
  }

}
