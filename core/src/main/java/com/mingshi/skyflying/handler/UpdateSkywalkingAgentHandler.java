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
public class UpdateSkywalkingAgentHandler implements UpdateOperationRecord {
    @Override
    public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
        if (null != jsonNodes && methodName.equals(Const.UPDATE_SKYWALKING_AGENT) && null != jsonNodes.get(Const.AGENT_NAME)) {
            StringBuilder stringBuilder = new StringBuilder();
            String value = UpdateOperationRecordFactory.getValue(methodName, Const.AGENT_NAME);
            stringBuilder.append(value);
            String agentName = null;
            if (null != jsonNodes.get(Const.AGENT_CODE)) {
                agentName = jsonNodes.get(Const.AGENT_CODE).asText();
                stringBuilder.append(Const.COLON);
                stringBuilder.append(jsonNodes.get(Const.AGENT_NAME).asText());
            }
            operationLog.setOperationDesc(StringUtil.isBlank(agentName) == true ? stringBuilder.toString() : stringBuilder.toString().replace(Const.POUND_KEY, agentName));
        }
    }
}
