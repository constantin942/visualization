package com.mingshi.skyflying.handler;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;

/**
 * <B>类名称：SetAkSkHandler</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022-11-02 09:13:03
 * @Param
 * @return
 **/
public class UpdateUserFromHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && methodName.equals(Const.UPDATE_USER_FROM)) {
        JsonNode ruleId = jsonNodes.get(Const.RULE_ID);
        JsonNode isDelete = jsonNodes.get(Const.IS_DELETE);
        StringBuilder stringBuilder = new StringBuilder();
        if(null != ruleId && null != isDelete){
            stringBuilder.append(isDelete.asText().equals(0)  == true ? Const.ENABLE : Const.DISABLE);
            stringBuilder.append(Const.RULE_ID_DESC + ruleId.asText() + Const.DE);
        }
        stringBuilder.append(Const.UPDATE_USER_FROM_DESC);
        operationLog.setOperationDesc(stringBuilder.toString());
    }
  }
}
