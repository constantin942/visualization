package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.dao.OperateLogMapper;
import com.mingshi.skyflying.service.OperationLogService;
import jodd.util.StringUtil;
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
public class OperationLogServiceImpl extends ParentServiceImpl<OperationLog, Long> implements OperationLogService {

  @Resource
  private OperateLogMapper operateLogMapper;

  @Override
  public ServerResponse<String> getOperationLog(String userName, Integer pageNo, Integer pageSize) {
    Map<String, Object> hashMap = new HashMap();
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    hashMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
    hashMap.put(Const.PAGE_SIZE, pageSize);
    if (StringUtil.isNotBlank(userName)) {
      hashMap.put(Const.USER_NAME, userName);
    }
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    List<OperationLog> operationLogs = operateLogMapper.selectAllOperationLog(hashMap);
    if (null != operationLogs && 0 < operationLogs.size()) {
      bySuccess.setData(JsonUtil.obj2String(operationLogs));
    }
    log.info("# OperationLogServiceImpl.getOperationLog() # 根据查询条件【{}】，获取高危操作日志成功。", hashMap);
    return bySuccess;
  }
}
