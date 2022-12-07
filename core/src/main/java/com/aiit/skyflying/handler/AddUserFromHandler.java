package com.aiit.skyflying.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.OperationLog;

/**
 * <B>类名称：SetAkSkHandler</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022-11-02 09:13:03
 * @Param
 * @return
 **/
public class AddUserFromHandler implements UpdateOperationRecord {
    @Override
    public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
        if (null != jsonNodes && methodName.equals(Const.ADD_USER_FROM)) {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append(Const.ADD_USER_FROM_DESC);
            JsonNode userFromPath = jsonNodes.get(Const.USER_FROM_PATH);
            JsonNode userFromDesc = jsonNodes.get(Const.USER_FROM_DESC);
            if (null != userFromPath && null != userFromDesc) {
                stringBuilder.append(userFromPath.asText());
                stringBuilder.append(Const.POUND_KEY);
                stringBuilder.append(userFromDesc.asText());
            }
            operationLog.setOperationDesc(stringBuilder.toString());
        }
    }
}
