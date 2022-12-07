package com.aiit.skyflying.impl;

import com.aiit.skyflying.common.agent.AgentInformationSingleton;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.MsAgentInformationDo;
import com.aiit.skyflying.common.domain.MsAgentSwitchDo;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.utils.DateTimeUtil;
import com.aiit.skyflying.common.utils.JsonUtil;
import com.aiit.skyflying.common.utils.RedisPoolUtil;
import com.aiit.skyflying.common.utils.StringUtil;
import com.aiit.skyflying.common.dao.MsAgentInformationMapper;
import com.aiit.skyflying.dao.MsAgentSwitchMapper;
import com.aiit.skyflying.service.MsAgentInformationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;

/**
 * <B>方法名称：MsAgentInformationServiceImpl</B>
 * <B>概要说明：操作探针的实现类</B>
 *
 * @Author zm
 * @Date 2022年06月28日 16:06:31
 * @Param
 * @return
 **/
@Slf4j
@Service("msAgentInformationService")
public class MsAgentInformationServiceImpl implements MsAgentInformationService {
  @Resource
  private MsAgentInformationMapper msAgentInformationMapper;
  @Resource
  private MsAgentSwitchMapper msAgentSwitchMapper;
  @Resource
  private RedisPoolUtil redisPoolUtil;

  @Override
  public ServerResponse<String> getAllSkywalkingAgent(String agentCode, Integer pageNo, Integer pageSize) {
    ServerResponse<String> bySuccess = null;
    try {
      Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
      if (StringUtil.isNotBlank(agentCode)) {
        queryMap.put("agentCode", agentCode);
      }
      if (null == pageNo) {
        pageNo = 1;
      }
      if (null == pageSize) {
        pageSize = 10;
      }
      queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
      queryMap.put(Const.PAGE_SIZE, pageSize);

      List<MsAgentInformationDo> userPortraitRulesDoList = msAgentInformationMapper.selectAllAgents(queryMap);
      log.info("执行 # MsAgentInformationServiceImpl.getAllSkywalkingAgent() # 获取所有的探针信息。根据查询条件【{}】获取到的探针信息是【{}】。", JsonUtil.obj2String(queryMap), JsonUtil.obj2String(userPortraitRulesDoList));

      Integer count = msAgentInformationMapper.selectAllAgentsCount(queryMap);
      Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
      bySuccess = ServerResponse.createBySuccess();
      context.put("rows", JsonUtil.obj2String(userPortraitRulesDoList));
      context.put("total", count);
      bySuccess.setData(JsonUtil.obj2String(context));
    } catch (Exception e) {
      log.info(" # MsAgentInformationServiceImpl.getAllSkywalkingAgent() # 获取所有的探针信息时，出现了异常。", e);
    }
    log.info("执行完毕 # MsAgentInformationServiceImpl.getAllSkywalkingAgent() # 获取所有的探针信息。");
    return bySuccess;
  }

  @Override
  public ServerResponse<String> updateSkywalkingAgent(Integer id, String agentName) {
    if (null == id) {
      return ServerResponse.createByErrorMessage("规则id不能为空。", "");
    }
    if (StringUtil.isBlank(agentName)) {
      return ServerResponse.createByErrorMessage("规则别名不能为空。", "");
    }
    MsAgentInformationDo msAgentInformationDo = msAgentInformationMapper.selectByPrimaryKey(id);
    if(null == msAgentInformationDo){
      return ServerResponse.createByErrorMessage("数据不存在。", "");
    }
    msAgentInformationDo.setAgentName(agentName);
    int updateResult = msAgentInformationMapper.updateByPrimaryKeySelective(msAgentInformationDo);
    if (1 != updateResult) {
      return ServerResponse.createByErrorMessage("更新失败。", "");
    }
    String agentCode = msAgentInformationDo.getAgentCode();
    if(StringUtil.isNotBlank(agentCode)){
      AgentInformationSingleton.put(agentCode, agentName);
    }
    return ServerResponse.createBySuccess();
  }

  @Override
  public ServerResponse<String> getActiveSkywalkingAgent() {
    List<Map<String, String>> list = new LinkedList<>();
    Map<Object, Object> hgetall = redisPoolUtil.hgetall(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST);
    if (null != hgetall && 0 < hgetall.size()) {
      Iterator<Object> iterator = hgetall.keySet().iterator();
      while (iterator.hasNext()) {
        String key = String.valueOf(iterator.next());
        String stringDate = String.valueOf(hgetall.get(key));
        long interval = DateTimeUtil.getSecondByDate(stringDate);

        doActiveSkywalkingAgent(list, interval, key, stringDate);
      }
    }
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(list));
    log.info(" 执行完毕 # SkyflyingController.getActiveSkywalkingAgent() # 从Redis中获取当前存活的探针数量是【{}】。", list.size());
    return bySuccess;
  }

  private void doActiveSkywalkingAgent(List<Map<String, String>> list, long interval, String key,String date) {
    if (interval > Const.SKYWALKING_AGENT_HEART_BEAT_INTERVAL_SECONDS) {
        redisPoolUtil.hDelete(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, key);
        return;
    }
      Map<String, String> codeNameMap = JsonUtil.string2Obj(key, Map.class);
      if (null != codeNameMap) {
          Map<String, String> map = new HashMap<>(Const.NUMBER_EIGHT);
          String serviceCode = codeNameMap.get("serviceCode");
          String serviceInstanceName = codeNameMap.get("serviceInstanceName");
          map.put("serviceCode", serviceCode);
          String agentName = msAgentInformationMapper.selectByAgentCode(serviceCode);
          map.put("agentName", agentName);
          map.put("serviceInstanceName", serviceInstanceName);
          map.put("time", date);

          // 获取探针状态；
          MsAgentSwitchDo msAgentSwitchDo = msAgentSwitchMapper.selectByServiceInstanceLatest(serviceInstanceName);
          if(null != msAgentSwitchDo){
              String agentSwitch = msAgentSwitchDo.getAgentSwitchStatus();
              String sendKafkaStatus = msAgentSwitchDo.getSendKafkaStatus();
              String receiveKafkaStatus = msAgentSwitchDo.getReceiveKafkaStatus();
              if(Const.SUCCESS.equals(sendKafkaStatus) && Const.SUCCESS.equals(receiveKafkaStatus)){
                  map.put(Const.STATUS, agentSwitch);
              }else{
                  map.put(Const.STATUS, Const.AGENT_STATUS_UNKNOWN);
              }
          }else{
              map.put(Const.STATUS, Const.AGENT_STATUS_ON);
          }
          list.add(map);
      }
  }
}
