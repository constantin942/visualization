package com.mingshi.skyflying.handler;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;

/**
 * <B>类名称：SetRegionHandler</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022-11-02 09:13:03
 * @Param
 * @return
 **/
public class SetRegionHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && methodName.equals(Const.SET_REGION)) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(Const.SET_REGION_DESC);
        operationLog.setOperationDesc(stringBuilder.toString());
    }
  }
}
