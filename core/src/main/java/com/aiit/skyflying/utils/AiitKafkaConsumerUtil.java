package com.aiit.skyflying.utils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.MsAgentSwitchDo;
import com.aiit.skyflying.common.domain.SendStateRecord;
import com.aiit.skyflying.common.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.aiit.skyflying.common.reactor.thread.ProcessorThread;
import com.aiit.skyflying.common.service.SegmentConsumerService;
import com.aiit.skyflying.common.utils.JsonUtil;
import com.aiit.skyflying.dao.MsAgentSwitchMapper;
import com.aiit.skyflying.dao.MsExceptionInfoMapper;
import com.aiit.skyflying.dao.SendStateRecordMapper;
import com.aiit.skyflying.domain.MsExceptionInfo;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: AiitKafkaConsumerUtil</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * Date 2022/7/28 17:02
 * @Version 1.0
 **/
@Slf4j
@Component
public class AiitKafkaConsumerUtil {
    @Resource
    private SegmentConsumerService segmentConsumerService;
    @Resource
    private MsAgentSwitchMapper msAgentSwitchMapper;
    @Resource
    private MsExceptionInfoMapper msExceptionInfoMapper;
    @Resource
    private SendStateRecordMapper sendStateRecordMapper;

    /**
     * <B>方法名称：handleExceptionInfo</B>
     * <B>概要说明：将探针发来的异常信息保存到数据库中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-24 14:59:45
     * @Param [jsonNodes]
     **/
    public void handleExceptionInfo(ObjectNode jsonNodes) {
        try {
            JsonNode body = jsonNodes.get("body");
            if (null == body) {
                return;
            }
            ObjectNode jsonObject = JsonUtil.string2Obj(body.asText(), ObjectNode.class);
            JsonNode exceptionInfo = jsonObject.get("exceptionInfo");
            JsonNode serviceInstanceName = jsonObject.get("serviceInstanceName");
            JsonNode serviceCode = jsonObject.get("serviceCode");
            MsExceptionInfo msExceptionInfo = new MsExceptionInfo();
            if (null != exceptionInfo) {
                msExceptionInfo.setExceptionInfo(exceptionInfo.asText());
            }
            if (null != serviceInstanceName) {
                msExceptionInfo.setServiceInstanceName(serviceInstanceName.asText());
            }
            if (null != serviceCode) {
                msExceptionInfo.setServiceCode(serviceCode.asText());
            }
            msExceptionInfoMapper.insertSelective(msExceptionInfo);
        } catch (Exception e) {
            log.error("# AiitKafkaConsumerUtil.handleExceptionInfo() # 将探针发来的异常信息保存到数据库中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：handleSendRecordsState</B>
     * <B>概要说明：保存探针已发送成功的链路信息，用于排查网关发送消息丢失，导致用户名没有的情况</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-24 15:31:47
     * @Param [jsonNodes]
     **/
    public void handleSendRecordsState(ObjectNode jsonNodes) {
        try {
            JsonNode body = jsonNodes.get("body");
            if (null == body) {
                return;
            }
            List<SendStateRecord> sendStateRecordList = JsonUtil.string2Obj(body.asText(), List.class, SendStateRecord.class);
            if (!sendStateRecordList.isEmpty()) {
                sendStateRecordMapper.insertSelectiveBatch(sendStateRecordList);
            }
        } catch (Exception e) {
            log.error("# AiitKafkaConsumerUtil.handleSendRecordsState() # 将探针发来的异常信息保存到数据库中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：updateMsAgentSwitchStatus</B>
     * <B>概要说明：更新探针的状态</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月25日 14:08:10
     * @Param [value]
     **/
    public void updateMsAgentSwitchStatus(String value) {
        try {
            ObjectNode jsonNodes = JsonUtil.string2Obj(value, ObjectNode.class);
            String requestId = null;
            if (null == jsonNodes.get(Const.REQUEST_ID)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatus() # 从Kafka中获取探针返回的信息【{}】时，没有获取到请求id（request_id）参数。", value);
                return;
            }
            if (null == jsonNodes.get(Const.AGENT_OPERATION_TYPE)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatus() # 从Kafka中获取探针返回的信息【{}】时，没有获取到请求类型（{}）参数。", value, Const.AGENT_OPERATION_TYPE);
                return;
            }
            requestId = jsonNodes.get(Const.REQUEST_ID).asText();
            String responseStatus = null;
            if (null == jsonNodes.get(Const.RESPONSE_STATUS)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatus() # 从Kafka中获取探针返回的信息【{}】时，没有获取到探针操作状态（status）参数。", value);
                return;
            }
            responseStatus = jsonNodes.get(Const.RESPONSE_STATUS).asText();

            MsAgentSwitchDo msAgentSwitchDo = new MsAgentSwitchDo();
            msAgentSwitchDo.setRequestId(requestId);
            msAgentSwitchDo.setReceiveKafkaResponseParams(value);
            msAgentSwitchDo.setReceiveKafkaStatus(responseStatus);

            String operationType = jsonNodes.get(Const.AGENT_OPERATION_TYPE).asText();
            if (Const.AGENT_QUERY.equals(operationType) && null != jsonNodes.get(Const.AGENT_STATUS)) {
                String agentStatus = jsonNodes.get(Const.AGENT_STATUS).asText();
                msAgentSwitchDo.setAgentSwitchStatus(Const.TRUE.equals(agentStatus) ? Const.AGENT_STATUS_ON : Const.AGENT_STATUS_OFF);
            }

            Integer result = msAgentSwitchMapper.updateByRequestId(msAgentSwitchDo);
            if (!Const.NUMBER_ONE.equals(result)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatus() # 更新探针【{}】的状态失败。", value);
            }
        } catch (Exception e) {
            log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatus() # 更新探针【{}】的状态时，出现了异常。", value, e);
        }
    }

    public void updateMsAgentSwitchStatusOld(ObjectNode jsonNodes) {
        String value = jsonNodes.toString();
        try {
            String requestId = null;
            if (null == jsonNodes.get(Const.REQUEST_ID)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatusOld() # 从Kafka中获取探针返回的信息【{}】时，没有获取到请求id（request_id）参数。", value);
                return;
            }
            if (null == jsonNodes.get(Const.AGENT_OPERATION_TYPE)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatusOld() # 从Kafka中获取探针返回的信息【{}】时，没有获取到请求类型（{}）参数。", value, Const.AGENT_OPERATION_TYPE);
                return;
            }
            requestId = jsonNodes.get(Const.REQUEST_ID).asText();
            String responseStatus = null;
            if (null == jsonNodes.get(Const.RESPONSE_STATUS)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatusOld() # 从Kafka中获取探针返回的信息【{}】时，没有获取到探针操作状态（status）参数。", value);
                return;
            }
            responseStatus = jsonNodes.get(Const.RESPONSE_STATUS).asText();

            MsAgentSwitchDo msAgentSwitchDo = new MsAgentSwitchDo();
            msAgentSwitchDo.setRequestId(requestId);
            msAgentSwitchDo.setReceiveKafkaResponseParams(value);
            msAgentSwitchDo.setReceiveKafkaStatus(responseStatus);

            String operationType = jsonNodes.get(Const.AGENT_OPERATION_TYPE).asText();
            if (Const.AGENT_QUERY.equals(operationType) && null != jsonNodes.get(Const.AGENT_STATUS)) {
                String agentStatus = jsonNodes.get(Const.AGENT_STATUS).asText();
                msAgentSwitchDo.setAgentSwitchStatus(Const.TRUE.equals(agentStatus) ? Const.AGENT_STATUS_ON : Const.AGENT_STATUS_OFF);
            }

            Integer result = msAgentSwitchMapper.updateByRequestId(msAgentSwitchDo);
            if (!Const.NUMBER_ONE.equals(result)) {
                log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatusOld() # 更新探针【{}】的状态失败。", value);
            }
        } catch (Exception e) {
            log.error("# MsKafkaAgentSwitchConsumer.updateMsAgentSwitchStatusOld() # 更新探针【{}】的状态时，出现了异常。", value, e);
        }
    }

    public void doOnMessage(ConsumerRecord<String, Bytes> consumerRecord) {
        // 使用Reactor模式；
        // 使用LinkedBlockingQueue两把锁队列；2022-07-22 20:57:19
        useReactorModelByLinkedBlockingQueue(consumerRecord);
    }

    /**
     * <B>方法名称：useReactorModelByLinkedBlockingQueueByGracefulShutdown</B>
     * <B>概要说明：不启用优雅关机的方式</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月27日 09:09:28
     * @Param [consumerRecord]
     **/
    private void useReactorModelByLinkedBlockingQueue(ConsumerRecord<String, Bytes> consumerRecord) {
        // 等待创建processor线程；2022-06-01 09:20:19
        waitingCreateProcessorsThread();
        try {
            ProcessorThread processorThread = null;
            Boolean offerResult = false;
            while (false == offerResult) {
                processorThread = InitProcessorByLinkedBlockingQueue.getProcessor();
                offerResult = processorThread.offer(consumerRecord);
            }
        } catch (Throwable e) {
            log.error("# AiitKafkaConsumerUtil.useReactorModelByLinkedBlockingQueue() # 消费者线程将拉取到的流量信息分发给processor线程时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：waitingCreateProcessorsThread</B>
     * <B>概要说明：等待创建processor线程</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年06月01日 09:06:29
     * @Param []
     **/
    private static void waitingCreateProcessorsThread() {
        try {
            while (Boolean.FALSE.equals(InitProcessorByLinkedBlockingQueue.getCreateProcessorsFinishedFlag())) {
                log.error("processor线程还没有创建完毕，等待一会。");
                TimeUnit.SECONDS.sleep(50);
            }
        } catch (Exception e) {
            log.error("在消费者程序中，等待创建processor线程完毕时，出现了异常。", e);
        }
    }
}
