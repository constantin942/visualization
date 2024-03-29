package com.aiit.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.OperationLog;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.utils.JsonUtil;
import com.aiit.skyflying.dao.OperateLogMapper;
import com.aiit.skyflying.service.OperationLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：OperationLogServiceImpl</B>
 * <B>概要说明：获取高危操作日志实现类</B>
 *
 * @Author zm
 * @Date 2022年09月09日 16:09:59
 * @Param
 * @return
 **/
@Slf4j
@Service("operationLogService")
public class OperationLogServiceImpl extends BaseParentServiceImpl<OperationLog, Long> implements OperationLogService {

  @Resource
  private OperateLogMapper operateLogMapper;

  @Override
  public ServerResponse<String> getOperationLog(String userName, Integer pageNo, Integer pageSize) {
    Map<String, Object> hashMap = new HashMap<>(Const.INITAL_SIZE);
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    hashMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
    hashMap.put(Const.PAGE_SIZE, pageSize);
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    List<OperationLog> list = operateLogMapper.selectAllOperationLog(hashMap);

    Integer count = operateLogMapper.selectAllOperationLogCount(hashMap);
    ObjectNode context = JsonUtil.createJsonObject();
    if (null != list && !list.isEmpty()) {
      context.put("rows", JsonUtil.object2String(list));
    }
    context.put("total", count);
    bySuccess.setData(JsonUtil.object2String(context));

    log.info("# OperationLogServiceImpl.getOperationLog() # 根据查询条件【{}】，获取高危操作日志成功。", hashMap);
    return bySuccess;
  }
}
