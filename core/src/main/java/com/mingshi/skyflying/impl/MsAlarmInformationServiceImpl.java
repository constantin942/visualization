package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.dao.MsAlarmInformationMapper;
import com.mingshi.skyflying.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.MsAlarmInformationService;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：MsAlarmInformationServiceImpl</B>
 * <B>概要说明：操作告警信息的实现类</B>
 *
 * @Author zm
 * @Date 2022年05月25日 14:05:13
 * @Param
 * @return
 **/
@Slf4j
@Service("msAlarmInformationService")
public class MsAlarmInformationServiceImpl implements MsAlarmInformationService {
  @Resource
  private MsAlarmInformationMapper msAlarmInformationMapper;

  @Override
  public ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName, Integer matchRuleId, String originalTime, Integer pageNo, Integer pageSize) {
    Map<String, Object> queryMap = new HashMap<>();
    if (StringUtil.isNotBlank(userName)) {
      queryMap.put("userName", userName);
    }
    if (null != matchRuleId) {
      queryMap.put("matchRuleId", matchRuleId);
    }
    if (StringUtil.isNotBlank(originalTime)) {
      queryMap.put("originalTime", originalTime);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);
    List<MsAlarmInformationDo> alarmInformationDoList = msAlarmInformationMapper.selectAll(queryMap);

    Integer count = msAlarmInformationMapper.selectAllCount(queryMap);

    Map<String, Object> context = new HashMap<>();
    context.put("rows", JsonUtil.obj2String(alarmInformationDoList));
    context.put("total", count);

    log.info("# MsAlarmInformationServiceImpl.getAllAlarmInfo() # 获取所有的告警信息，根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), alarmInformationDoList.size());
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(context));
    return bySuccess;
  }

  @Override
  public ServerResponse<String> getAllAlarmInfo(String userName, Integer pageNo, Integer pageSize) {

    Map<String, Object> queryMap = new HashMap<>();
    if (StringUtil.isNotBlank(userName)) {
      queryMap.put("userName", userName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);
    List<Map<String, Object>> alarmInformationDoList = msAlarmInformationMapper.selectAllUserTimes(queryMap);

    Integer count = msAlarmInformationMapper.selectAllUserTimesCount(queryMap);

    Map<String, Object> context = new HashMap<>();
    context.put("rows", JsonUtil.obj2String(alarmInformationDoList));
    context.put("total", count);

    log.info("# MsAlarmInformationServiceImpl.getAllAlarmInfo() # 获取所有的告警信息，根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), alarmInformationDoList.size());
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(context));
    return bySuccess;
  }

  @Override
  public ServerResponse<String> getUserNameAnomalyDetectionInfo() {
    List<String> userNameList = msAlarmInformationMapper.selectAllUserName();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(userNameList));
    return bySuccess;
  }

  @Override
  public ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(Integer pageNo, Integer pageSize) {
    Map<String, Object> queryMap = new HashMap<>();
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);
    List<Map<String, Object>> alarmInformationDoList = msAlarmInformationMapper.selectAllByGroupByUserName(queryMap);

    Integer count = msAlarmInformationMapper.selectAllByGroupByUserNameCount();

    Map<String, Object> context = new HashMap<>();
    context.put("rows", JsonUtil.obj2String(alarmInformationDoList));
    context.put("total", count);

    log.info("# MsAlarmInformationServiceImpl.getAllAlarmInfo() # 获取所有的告警信息，根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), alarmInformationDoList.size());
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(context));
    return bySuccess;
  }
}
