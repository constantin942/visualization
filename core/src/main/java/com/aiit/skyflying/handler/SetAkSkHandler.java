package com.aiit.skyflying.handler;

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
public class SetAkSkHandler implements UpdateOperationRecord {
  @Override
  public void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog) {
    if (null != jsonNodes && methodName.equals(Const.SET_AK_SK)) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(Const.SET_AK_SK_DESC);
        operationLog.setOperationDesc(stringBuilder.toString());
    }
  }
}
