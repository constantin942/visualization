package com.mingshi.skyflying.handler;

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

  private UpdateOperationRecordFactory(){}
  static {
    Map<String, Object> updateMonitorTableMap = new ConcurrentHashMap<>();
    updateMonitorTableMap.put(Const.NUMBER_ONE_STR, "禁用表");
    updateMonitorTableMap.put(Const.NUMBER_ZERO_STR, "启用表");
    updateMonitorTableMap.put(Const.HANDLER, new UpdateMonitorTableHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_MONITOR_TABLE, updateMonitorTableMap);

    Map<String, Object> updateMonitorTableaDescMap = new ConcurrentHashMap<>();
    updateMonitorTableaDescMap.put(Const.TABLE_DESC, "给表（" + Const.POUND_KEY + "）起个中文名字");
    updateMonitorTableaDescMap.put(Const.HANDLER, new UpdateMonitorTableDescHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_MONITOR_TABLE_DESC, updateMonitorTableaDescMap);

    Map<String, Object> updateSkywalkingAgentDescMap = new ConcurrentHashMap<>();
    updateSkywalkingAgentDescMap.put(Const.AGENT_NAME, "给服务（" + Const.POUND_KEY + "）起个中文名字");
    updateSkywalkingAgentDescMap.put(Const.HANDLER, new UpdateSkywalkingAgentHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_SKYWALKING_AGENT, updateSkywalkingAgentDescMap);

    Map<String, Object> updateSkywalkingAgentOnAndOffMap = new ConcurrentHashMap<>();
    updateSkywalkingAgentOnAndOffMap.put(Const.AGENT_SWITCH, "更新探针（" + Const.POUND_KEY + "）状态");
    updateSkywalkingAgentOnAndOffMap.put(Const.HANDLER, new UpdateAgentStatusHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_SKYWALKING_AGENT_STATUS, updateSkywalkingAgentOnAndOffMap);

    Map<String, Object> setAkSkMap = new ConcurrentHashMap<>();
    setAkSkMap.put(Const.HANDLER, new SetAkSkHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.SET_AK_SK, setAkSkMap);

    Map<String, Object> setRegionMap = new ConcurrentHashMap<>();
    setRegionMap.put(Const.HANDLER, new SetRegionHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.SET_REGION, setRegionMap);

    Map<String, Object> updateDingConfigMap = new ConcurrentHashMap<>();
    updateDingConfigMap.put(Const.HANDLER, new DingdingAlarmConfigHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_DINGDING_CONFIG, updateDingConfigMap);

    Map<String, Object> changePasswordMap = new ConcurrentHashMap<>();
    changePasswordMap.put(Const.HANDLER, new ChangePasswrodHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.USER_CHANGE_PASSWORD, changePasswordMap);

    Map<String, Object> updateHighRiskOptMap = new ConcurrentHashMap<>();
    updateHighRiskOptMap.put(Const.HANDLER, new UpdateHighRiskOptHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_HIGH_RISK_OPT, updateHighRiskOptMap);

    Map<String, Object> updatePortraitConfigMap = new ConcurrentHashMap<>();
    updatePortraitConfigMap.put(Const.HANDLER, new UudatePortraitConfigHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_PORTRAIT_CONFIG, updatePortraitConfigMap);

    Map<String, Object> updateUserPortraitRuleMap = new ConcurrentHashMap<>();
    updateUserPortraitRuleMap.put(Const.HANDLER, new UudateUserPortraitRuleHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_USER_PORTRAIT_RULE, updateUserPortraitRuleMap);

    Map<String, Object> updateUuserFromMap = new ConcurrentHashMap<>();
    updateUuserFromMap.put(Const.HANDLER, new UpdateUserFromHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.UPDATE_USER_FROM, updateUuserFromMap);

    Map<String, Object> addUuserFromMap = new ConcurrentHashMap<>();
    addUuserFromMap.put(Const.HANDLER, new AddUserFromHandler());
    UPDATE_OPERATION_RECORD_MAP.put(Const.ADD_USER_FROM, addUuserFromMap);

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
