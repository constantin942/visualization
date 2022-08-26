package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsAgentSwitchDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.SnowflakeIdWorker;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsAgentSwitchMapper;
import com.mingshi.skyflying.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.service.MsAgentSwitchService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;

/**
 * <B>主类名称: MsAgentSwitchServiceImpl</B>
 * <B>概要说明：对探针进行开关</B>
 * Author zm
 * Date 2022/8/25 10:26
 *
 * @Version 1.0
 **/
@Slf4j
@Service("msAgentSwitchService")
public class MsAgentSwitchServiceImpl implements MsAgentSwitchService {

  @Value("${spring.kafka.consumer.agent-switch-request-topic}")
  private String agentSwitchRequestTopic;

  @Resource
  private AiitKafkaProducer aiitKafkaProducer;
  @Resource
  private MsAgentSwitchMapper msAgentSwitchMapper;

  @Override
  public ServerResponse<String> allAgentOperationRecord(String serviceInstance, Integer pageNo, Integer pageSize) {

    HashMap hashMap = new HashMap();
    // ObjectNode jsonObject = JsonUtil.createJsonObject();
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    hashMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
    hashMap.put(Const.PAGE_SIZE, pageSize);
    hashMap.put("serviceInstance", serviceInstance);

    List<MsAgentSwitchDo> list = msAgentSwitchMapper.selectByServiceInstance(hashMap);

    Integer count = msAgentSwitchMapper.selectByServiceInstanceCount(hashMap);
    ObjectNode context = JsonUtil.createJsonObject();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    if (null != list && !list.isEmpty()) {
      context.put("rows", JsonUtil.object2String(list));
    }
    context.put("total", count);
    bySuccess.setData(JsonUtil.object2String(context));

    return bySuccess;
  }

  @Override
  public ServerResponse<String> updateAgentStatus(String serviceInstance, String agentSwitch) {

    if (StringUtil.isBlank(serviceInstance)) {
      return ServerResponse.createByErrorMessage("探针唯一标识符(serviceInstance)不能为空", "");
    }
    if (StringUtil.isBlank(agentSwitch)) {
      return ServerResponse.createByErrorMessage("探针开关参数(agentSwitch)不能为空，只能是on或off", "");
    }

    if (!Const.ON.equals(agentSwitch) && !Const.OFF.equals(agentSwitch)) {
      return ServerResponse.createByErrorMessage("探针开关参数(agentSwitch)只能是on或off", "");
    }

    return sendToKafkaAndSaveOperationRecord(serviceInstance, agentSwitch, Const.AGENT_ON_OFF);
  }

  /**
   * <B>方法名称：sendToKafkaAndSaveOperationRecord</B>
   * <B>概要说明：给探针发送指令，并记录操作</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年08月26日 10:08:36
   * @Param [serviceInstance, agentSwitch]
   **/
  private ServerResponse<String> sendToKafkaAndSaveOperationRecord(String serviceInstance, String agentSwitch, String operationType) {
    String requestId = SnowflakeIdWorker.generateStringId();
    ObjectNode jsonNodes = JsonUtil.createJsonObject();
    jsonNodes.put("serviceInstance", serviceInstance);
    if (StringUtil.isNotBlank(agentSwitch)) {
      jsonNodes.put("agentSwitch", agentSwitch);
    }
    jsonNodes.put("operationType", operationType);
    jsonNodes.put("requestId", requestId);
    Boolean sendResult = aiitKafkaProducer.sendWithSpecifyPartion(agentSwitchRequestTopic, jsonNodes);
    if (false == sendResult) {
      if (operationType.equals(Const.AGENT_QUERY)) {
        log.error("# MsAgentSwitchServiceImpl.updateAgentStatus() # 对探针【{}】进行查询【{}】，操作失败。", serviceInstance, operationType);
      } else if (operationType.equals(Const.AGENT_ON_OFF)) {
        log.error("# MsAgentSwitchServiceImpl.updateAgentStatus() # 对探针【{}】进行开关【{}】设置，操作失败。", serviceInstance, operationType);
      }
      return ServerResponse.createByErrorMessage("操作失败，请稍后重试。", "");
    }

    // 将探针开关的记录插入到数据库
    insertMsAgentSwitchDo(serviceInstance, agentSwitch, requestId, sendResult, jsonNodes.toString(), operationType);

    if (operationType.equals(Const.AGENT_QUERY)) {
      log.info("# MsAgentSwitchServiceImpl.updateAgentStatus() # 对探针【{}】进行查询【{}】，操作成功。", serviceInstance, operationType);
    } else if (operationType.equals(Const.AGENT_ON_OFF)) {
      log.info("# MsAgentSwitchServiceImpl.updateAgentStatus() # 对探针【{}】进行开关【{}】设置，操作成功。", serviceInstance, agentSwitch);
    }
    return ServerResponse.createBySuccess();
  }

  @Override
  public ServerResponse<String> queryAgentStatus(String serviceInstance) {
    return sendToKafkaAndSaveOperationRecord(serviceInstance, null, Const.AGENT_QUERY);
  }

  /**
   * <B>方法名称：insertMsAgentSwitchDo</B>
   * <B>概要说明：将探针开关的记录插入到数据库</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月25日 13:08:01
   * @Param [serviceInstance, agentSwitch, requestId, sendResult, sendKafkaRequestParams]
   **/
  private void insertMsAgentSwitchDo(String serviceInstance, String agentSwitch, String requestId, Boolean sendResult, String sendKafkaRequestParams, String operationType) {
    try {
      MsAgentSwitchDo msAgentSwitchDo = new MsAgentSwitchDo();
      if(StringUtil.isNotBlank(agentSwitch)){
        msAgentSwitchDo.setAgentSwitchStatus(agentSwitch);
      }
      msAgentSwitchDo.setSendKafkaRequestParams(sendKafkaRequestParams);
      msAgentSwitchDo.setServiceInstance(serviceInstance);
      msAgentSwitchDo.setRequestId(requestId);
      msAgentSwitchDo.setOperationType(operationType);
      if (sendResult.equals(false)) {
        msAgentSwitchDo.setSendKafkaStatus(Const.FAILED);
      } else {
        msAgentSwitchDo.setSendKafkaStatus(Const.SUCCESS);
      }
      Integer result = msAgentSwitchMapper.insertSelective(msAgentSwitchDo);
      if (!Const.NUMBER_ONE.equals(result)) {
        log.error("# MsAgentSwitchServiceImpl.insertMsAgentSwitchDo() # 将对探针的记录【{}】插入到数据库失败。", JsonUtil.object2String(msAgentSwitchDo));
      }
    } catch (Exception e) {
      log.error("# MsAgentSwitchServiceImpl.insertMsAgentSwitchDo() # 将探针开关的记录【{}】插入到数据库中，出现了异常。", e);
    }
  }
}
