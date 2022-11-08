package com.mingshi.skyflying.handler;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;
import jodd.util.StringUtil;

/**
 * <B>主类名称: updateMonitorTableHandler</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/9/16 14:56
 *
 * @Version 1.0
 **/
public class UpdateAgentStatusHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && Const.UPDATE_SKYWALKING_AGENT_STATUS.equals(methodName) && null != jsonNodes.get(Const.SERVICE_INSTANCE)) {
        String serviceInstance = jsonNodes.get(Const.SERVICE_INSTANCE).asText();
        StringBuilder stringBuilder = new StringBuilder();
        String value = UpdateOperationRecordFactory.getValue(methodName, Const.AGENT_SWITCH);
        stringBuilder.append(value);
        String agentSwitch = null;
        if (null != jsonNodes.get(Const.AGENT_SWITCH)) {
          agentSwitch = jsonNodes.get(Const.AGENT_SWITCH).asText();
          stringBuilder.append(Const.COLON);
          stringBuilder.append(Const.AGENT_STATUS_OFF.equals(agentSwitch) == true ? Const.AGENT_STATUS_OFF_DESC : Const.AGENT_STATUS_ON_DESC);
        }
        operationLog.setOperationDesc(StringUtil.isBlank(agentSwitch) == true ? stringBuilder.toString() : stringBuilder.toString().replace(Const.POUND_KEY, serviceInstance));
    }
  }
}
