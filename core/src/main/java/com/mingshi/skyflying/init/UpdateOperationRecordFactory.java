package com.mingshi.skyflying.init;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;

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
public class UpdateOperationRecordFactory {
  private static final Map<String, Map<String, Object>> UPDATE_OPERATION_RECORD_MAP = new ConcurrentHashMap<>();

  static {
    Map<String, Object> updateMonitorTableMap = new ConcurrentHashMap<>();
    updateMonitorTableMap.put(Const.NUMBER_ONE_STR, "禁用表");
    updateMonitorTableMap.put(Const.NUMBER_ZERO_STR, "启用表");
    updateMonitorTableMap.put(Const.HANDLER, new UpdateMonitorTableHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_MONITOR_TABLE, updateMonitorTableMap);

    Map<String, Object> updateMonitorTableaDescMap = new ConcurrentHashMap<>();
    updateMonitorTableaDescMap.put(Const.TABLE_DESC, "给表（" + Const.COMMA + "）起个中文名字");
    updateMonitorTableaDescMap.put(Const.HANDLER, new UpdateMonitorTableDescHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_MONITOR_TABLE_DESC, updateMonitorTableaDescMap);

    Map<String, Object> updateSkywalkingAgentDescMap = new ConcurrentHashMap<>();
    updateSkywalkingAgentDescMap.put(Const.AGENT_NAME, "给服务（" + Const.COMMA + "）起个中文名字");
    updateSkywalkingAgentDescMap.put(Const.HANDLER, new UpdateSkywalkingAgentHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_SKYWALKING_AGENT, updateSkywalkingAgentDescMap);

    Map<String, Object> updateSkywalkingAgentOnAndOffMap = new ConcurrentHashMap<>();
    updateSkywalkingAgentOnAndOffMap.put(Const.AGENT_SWITCH, "更新探针（" + Const.COMMA + "）状态");
    updateSkywalkingAgentOnAndOffMap.put(Const.HANDLER, new UpdateAgentStatusHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_SKYWALKING_AGENT_STATUS, updateSkywalkingAgentOnAndOffMap);
  }

  public static String getValue(String key, String opreation) {
    Map<String, Object> map = UPDATE_OPERATION_RECORD_MAP.get(key);
    if (null != map && map.containsKey(opreation)) {
      return (String) map.get(opreation);
    }
    return null;
  }

  public static void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    Map<String, Object> map = UPDATE_OPERATION_RECORD_MAP.get(methodName);
    if (null != map) {
      UpdateOperationRecord updateOperationRecord = (UpdateOperationRecord) map.get(Const.HANDLER);
      if (null != updateOperationRecord) {
        updateOperationRecord.execute(jsonNodes, methodName, operationLog);
      }
    }
  }

}
