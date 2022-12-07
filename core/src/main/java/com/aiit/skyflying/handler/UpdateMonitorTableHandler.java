package com.aiit.skyflying.handler;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.OperationLog;

/**
 * <B>主类名称: updateMonitorTableHandler</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/9/16 14:56
 *
 * @Version 1.0
 **/
public class UpdateMonitorTableHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes.get(Const.IS_DELETE_OPRETION)) {
      StringBuilder stringBuilder = new StringBuilder();
      String value = UpdateOperationRecordFactory.getValue(methodName, jsonNodes.get(Const.IS_DELETE_OPRETION).asText());
      stringBuilder.append(value);
      if (null != jsonNodes.get(Const.TABLE_NAME)) {
        stringBuilder.append(Const.COLON);
        stringBuilder.append(jsonNodes.get(Const.TABLE_NAME).asText());
      }
      operationLog.setOperationDesc(stringBuilder.toString());
    }
  }
}
