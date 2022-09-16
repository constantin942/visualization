package com.mingshi.skyflying.init;

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
public class UpdateMonitorTableDescHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && methodName.equals(Const.UPDATE_MONITOR_TABLE_DESC)) {
      if (null != jsonNodes.get(Const.TABLE_DESC)) {
        StringBuilder stringBuilder = new StringBuilder();
        String value = UpdateOperationRecordMap.getValue(methodName, Const.TABLE_DESC);
        stringBuilder.append(value);
        String tableName = null;
        if (null != jsonNodes.get(Const.TABLE_NAME)) {
          tableName = jsonNodes.get(Const.TABLE_NAME).asText();
          stringBuilder.append(Const.COLON);
          stringBuilder.append(jsonNodes.get(Const.TABLE_DESC).asText());
        }
        operationLog.setOperationDesc(StringUtil.isBlank(tableName) == true ? stringBuilder.toString() : stringBuilder.toString().replace(Const.COMMA, tableName));
      }
    }
  }
}
