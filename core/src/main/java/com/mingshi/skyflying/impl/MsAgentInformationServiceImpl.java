package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.agent.AgentInformationSingleton;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsAgentInformationDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsAgentInformationMapper;
import com.mingshi.skyflying.service.MsAgentInformationService;
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
  private RedisPoolUtil redisPoolUtil;

  @Override
  public ServerResponse<String> getAllSkywalkingAgent(String agentCode, Integer pageNo, Integer pageSize) {
    ServerResponse<String> bySuccess = null;
    try {
      Map<String, Object> queryMap = new HashMap<>();
      if (StringUtil.isNotBlank(agentCode)) {
        queryMap.put("agentCode", agentCode);
      }
      if (null == pageNo) {
        pageNo = 1;
      }
      if (null == pageSize) {
        pageSize = 10;
      }
      queryMap.put("pageNo", (pageNo - 1) * pageSize);
      queryMap.put("pageSize", pageSize);

      List<MsAgentInformationDo> userPortraitRulesDoList = msAgentInformationMapper.selectAllAgents(queryMap);
      log.info("执行 # MsAgentInformationServiceImpl.getAllSkywalkingAgent() # 获取所有的探针信息。根据查询条件【{}】获取到的探针信息是【{}】。", JsonUtil.obj2String(queryMap), JsonUtil.obj2String(userPortraitRulesDoList));

      Integer count = msAgentInformationMapper.selectAllAgentsCount(queryMap);
      Map<String, Object> context = new HashMap<>();
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
    Integer count = 0;
    Map<Object, Object> hgetall = redisPoolUtil.hgetall(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST);
    if (null != hgetall && 0 < hgetall.size()) {
      Iterator<Object> iterator = hgetall.keySet().iterator();
      while (iterator.hasNext()) {
        String key = String.valueOf(iterator.next());
        String stringDate = String.valueOf(hgetall.get(key));
        long interval = DateTimeUtil.getSecondByDate(stringDate);

        doActiveSkywalkingAgent(list, interval, key, count, stringDate);
      }
    }
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(list));
    log.info(" 执行完毕 # SkyflyingController.getActiveSkywalkingAgent() # 从Redis中获取当前存活的探针数量是【{}】。", count);
    return bySuccess;
  }

  private void doActiveSkywalkingAgent(List<Map<String, String>> list, long interval, String key, Integer count, String date) {
    if (interval > Const.SKYWALKING_AGENT_HEART_BEAT_INTERVAL_SECONDS) {
      try {
        redisPoolUtil.hDelete(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, key);
      } catch (Exception e) {
        log.error(" # SkyflyingController.getActiveSkywalkingAgent() # 从Redis中获取当前存活的探针时，出现了异常。", e);
      }
    } else {
      ++count;
      Map<String, String> codeNameMap = JsonUtil.string2Obj(key, Map.class);
      if (null != codeNameMap) {
        Map<String, String> map = new HashMap<>();
        String serviceCode = codeNameMap.get("serviceCode");
        String serviceInstanceName = codeNameMap.get("serviceInstanceName");
        map.put("serviceCode", serviceCode);
        String agentName = msAgentInformationMapper.selectByAgentCode(serviceCode);
        map.put("agentName", agentName);
        map.put("serviceInstanceName", serviceInstanceName);
        map.put("time", date);
        list.add(map);
      }
    }
  }
}
