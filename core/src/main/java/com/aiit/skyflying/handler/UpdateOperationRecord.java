package com.aiit.skyflying.handler;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.aiit.skyflying.common.domain.OperationLog;

/**
 * <B>接口名称: UpdateOperationRecord</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/9/16 14:55
 *
 * @Version 1.0
 **/
public interface UpdateOperationRecord {
  void execute(ObjectNode jsonNodes, String methodName, OperationLog operationLog);
}
