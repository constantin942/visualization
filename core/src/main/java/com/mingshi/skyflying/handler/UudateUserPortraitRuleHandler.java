package com.mingshi.skyflying.handler;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;

/**
 * <B>类名称：UudateUserPortraitRuleHandler</B>
 * <B>概要说明：记录禁启用画像规则操作日志</B>
 *
 * @Author zm
 * @Date 2022-11-02 09:13:03
 * @Param
 * @return
 **/
public class UudateUserPortraitRuleHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && methodName.equals(Const.UPDATE_USER_PORTRAIT_RULE)) {
        StringBuilder stringBuilder = new StringBuilder();
        if(null != jsonNodes.get(Const.RULE_ID) && null != jsonNodes.get(Const.IS_DELETE)){
            String ruleId = jsonNodes.get(Const.RULE_ID).asText();
            Integer isDelete = jsonNodes.get(Const.IS_DELETE).asInt();
            if(isDelete.equals(Const.NUMBER_ZERO)){
                stringBuilder.append(Const.ENABLE);
                stringBuilder.append(Const.UPDATE_USER_PORTRAIT_RULE_DESC);
            }else if(isDelete.equals(Const.NUMBER_ONE)){
                stringBuilder.append(Const.DISABLE);
                stringBuilder.append(Const.UPDATE_USER_PORTRAIT_RULE_DESC);
            }
            stringBuilder.append(ruleId);
        }
        operationLog.setOperationDesc(stringBuilder.toString());
    }
  }
}
