package com.mingshi.skyflying.init;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;

/**
 * <B>类名称：UpdateHighRiskOptHandler</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022-11-02 09:13:03
 * @Param
 * @return
 **/
public class UpdateHighRiskOptHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && methodName.equals(Const.UPDATE_HIGH_RISK_OPT)) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(Const.UPDATE_HIGH_RISK_OPT_DESC);
        operationLog.setOperationDesc(stringBuilder.toString());
    }
  }
}
