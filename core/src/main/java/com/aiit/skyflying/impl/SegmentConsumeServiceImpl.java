package com.aiit.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.benmanes.caffeine.cache.Cache;
import com.google.protobuf.InvalidProtocolBufferException;
import com.aiit.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.aiit.skyflying.common.caffeine.MsCaffeine;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.*;
import com.aiit.skyflying.common.init.LoadAllEnableMonitorTablesFromDb;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.SegmentConsumerService;
import com.aiit.skyflying.common.statistics.InformationOverviewSingleton;
import com.aiit.skyflying.common.type.KeyValue;
import com.aiit.skyflying.common.type.LogEntity;
import com.aiit.skyflying.common.type.RefType;
import com.aiit.skyflying.common.utils.*;
import com.aiit.skyflying.component.ComponentsDefine;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.apache.skywalking.apm.network.language.agent.v3.SpanObject;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;

/**
 * <B>类名称：SegmentConsumeServiceImpl</B>
 * <B>概要说明：清洗调用链信息</B>
 *
 * @Author zm
 * @Date 2022年05月19日 17:05:07
 * @Param
 * @return
 **/
@Slf4j
@Service("SegmentConsumerService")
@PropertySource("classpath:application-${spring.profiles.active}.yml")
public class SegmentConsumeServiceImpl implements SegmentConsumerService {

    @Resource
    private MsCaffeine msCaffeine;
    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private AnomalyDetectionBusiness anomalyDetectionBusiness;

    @Override
    public ServerResponse<String> consume(ConsumerRecord<String, Bytes> consumerRecord) throws Exception {
        doConsume(consumerRecord);
        return null;
    }

    private void doConsume(ConsumerRecord<String, Bytes> consumerRecord) throws Exception {
        SegmentObject segmentObject = getSegmentObject(consumerRecord);

        HashSet<String> userHashSet = new HashSet<>();
        Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
        try {
            SegmentDo segment = new SegmentDo();
            // 设置segment_id、trace_id；2022-04-24 14:26:12
            getRef(segmentObject, segment);
            // 从SegmentObject实例中获取用户名和token；2022-07-12 10:22:53
            setUserNameAndTokenAndIpFromSegmentObject(userHashSet, segment, segmentObject);
            // 将用户名、token、globalTraceId放入到本地内存，并关联起来；2022-07-07 16:15:53
            setUserNameTokenGlobalTraceIdToLocalMemory(segment);

            // 存放用户名不为空的链路信息；
            LinkedList<MsSegmentDetailDo> segmentDetaiDolList = null;
            // 存放用户名为空的链路信息；
            LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList = null;
            // 获取探针的名称；2022-06-28 14:25:46
            skywalkingAgentHeartBeatMap = getAgentServiceName(segmentObject);

            List<Span> spanList = buildSpanList(segmentObject, segment);

            if (!spanList.isEmpty()) {
                // 组装segment；2022-04-20 16:33:48
                segment = setUserNameAndTokenFromSpan(spanList, segment);
                // 重组span数据，返回前端使用；2022-04-20 16:49:02
                reorganizingSpans(segment, spanList);
                // 存放用户名暂时为空的链路信息；
                segmentDetaiUserNameIsNullDolList = new LinkedList<>();
                // 组装msSegmentDetailDo实例信息，并放入到list集合中，然后方便下一步的批量处理
                segmentDetaiDolList = getSegmentDetaiDolList(segmentDetaiUserNameIsNullDolList, segment, segmentObject);
                // 异常检测；2022-10-13 09:40:57
                doUserVisitedIsAbnormal(segmentDetaiDolList);
            }

            HashMap<String, Integer> statisticsProcessorThreadQpsMap = statisticsProcessorThreadQps();

            // 使用reactor模型；2022-05-30 21:04:05
            mingshiServerUtil.doEnableReactorModel(statisticsProcessorThreadQpsMap, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, skywalkingAgentHeartBeatMap);
        } catch (Exception e) {
            log.error("清洗调用链信息时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：doUserVisitedIsAbnormal</B>
     * <B>概要说明：进行异常检测</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-13 09:41:04
     * @Param [segmentDetaiDolList, msAlarmInformationDoList]
     **/
    private void doUserVisitedIsAbnormal(LinkedList<MsSegmentDetailDo> segmentDetaiDolList) {
        Instant now = Instant.now();
        try {
            if (!segmentDetaiDolList.isEmpty()) {
                Boolean aBoolean = anomalyDetectionBusiness.userVisitedIsAbnormal(segmentDetaiDolList);
                long timeMillis = DateTimeUtil.getTimeMillis(now);
                if (Boolean.TRUE.equals(aBoolean) && Const.NUM_FIVE < timeMillis) {
                    log.info("# SegmentConsumeServiceImpl.doUserVisitedIsAbnormal() # 用户画像初始化成功，异常检测【{}条】耗时【{}】毫秒。", segmentDetaiDolList.size(), timeMillis);
                } else if (Const.NUM_FIVE < timeMillis) {
                    log.info("# SegmentConsumeServiceImpl.doUserVisitedIsAbnormal() # 用户画像初始化失败，将异常检测【{}条】发送到Kafka的生产者缓冲区中耗时【{}】毫秒。", segmentDetaiDolList.size(), timeMillis);
                }
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.doUserVisitedIsAbnormal() # 执行异常检测时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getSegmentObject</B>
     * <B>概要说明：从ConsumerRecord实例中获取SegmentObject实例</B>
     *
     * @return org.apache.skywalking.apm.network.language.agent.v3.SegmentObject
     * @Author zm
     * @Date 2022年09月13日 15:09:41
     * @Param [record]
     **/
    private SegmentObject getSegmentObject(ConsumerRecord<String, Bytes> consumerRecord) throws Exception {
        SegmentObject segmentObject = null;
        try {
            segmentObject = SegmentObject.parseFrom(consumerRecord.value().get());
        } catch (InvalidProtocolBufferException e) {
            log.error("# consume() # 消费skywalking探针发送来的数据时，出现了异常。", e);
            throw new RuntimeException();
        }
        return segmentObject;
    }

    /**
     * <B>方法名称：getAgentServiceName</B>
     * <B>概要说明：获取探针的名称</B>
     *
     * @return java.util.Map<java.lang.String, java.lang.String>
     * @Author zm
     * @Date 2022年06月28日 14:06:38
     * @Param [segmentObject]
     **/
    private Map<String, String> getAgentServiceName(SegmentObject segmentObject) {
        Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
        try {
            skywalkingAgentHeartBeatMap = new HashMap<>(Const.NUMBER_EIGHT);
            String service = segmentObject.getService();
            String serviceInstance = segmentObject.getServiceInstance();
            ObjectNode jsonObject = JsonUtil.createJsonObject();
            jsonObject.put(Const.SERVICE_CODE, service);
            jsonObject.put(Const.SERVICE_INSTANCE_NAME, serviceInstance);
            long segmentStartTime = segmentObject.getSegmentStartTime();
            String date = null;
            if (0L != segmentStartTime) {
                date = DateTimeUtil.longToDate(segmentStartTime);
            } else {
                date = DateTimeUtil.date2Str(new Date());
            }
            skywalkingAgentHeartBeatMap.put(jsonObject.toString(), date);

        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.getAgentServiceName() # 获取探针的名称时，出现了异常。", e);
        }
        return skywalkingAgentHeartBeatMap;
    }

    /**
     * <B>方法名称：getUserFrom</B>
     * <B>概要说明：从Kafka的消息中获取用户来源</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-23 17:38:19
     * @Param [operationName, msSegmentDetailDo]
     **/
    private void getUserFrom(String operationName, MsSegmentDetailDo msSegmentDetailDo, String parentEndpoint) {
        try {
            String str1 = "http://10.0.0.69:8181/login/web";
            String str2 = "https://hy2api.tian-wang.com/login/web";
            String str3 = "http://hy2api.tian-wang.com/login/web";
            String str4 = "http://hy4api.tian-wang.com/login/web";
            String str5 = "POST:/login/web";
            String str6 = "https://hy2api.tian-wang.com/login/app/merchantman";
            String str7 = "http://10.0.0.69:8181/login/fish/easier";

            if (null != operationName) {
//            if (null != operationName && (operationName.contains(str1) ||
//                operationName.contains(str2) ||
//                operationName.contains(str3) ||
//                operationName.contains(str4) ||
//                operationName.contains(str5) ||
//                operationName.contains(str6) ||
//                operationName.contains(str7)
//            )) {
                String userFromCacheByUserFromPath = doGetUserFromByPath(operationName, parentEndpoint);
                msSegmentDetailDo.setUserFrom(userFromCacheByUserFromPath);
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.getUserFrom() # 从Kafka的消息中获取用户来源时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：doGetUserFromByPath</B>
     * <B>概要说明：根据url地址获取用户来源</B>
     *
     * @return java.lang.String
     * @Author zm
     * @Date 2022-11-24 17:17:04
     * @Param [userFromPath]
     **/
    private String doGetUserFromByPath(String userFromPath, String parentEndpoint) {
        Instant now = Instant.now();
        String userFromName = null;
        Set<@NonNull String> keySet = msCaffeine.getUserFromMap().keySet();
        if (null != keySet && !keySet.isEmpty()) {
            for (int i = 0; i < keySet.size(); i++) {
                Iterator<@NonNull String> iterator = keySet.iterator();
                while (iterator.hasNext()) {
                    String key = iterator.next();
                    if (userFromPath.contains(key) || (StringUtil.isNotBlank(parentEndpoint)  && parentEndpoint.contains(key))) {
                        userFromName = msCaffeine.getUserFromCacheByUserFromPath(key);
                        log.info("# SegmentConsumeServiceImpl.doGetUserFromByPath() # 根据用户访问路径【{}】，获取用户对应的来源，用时【{}】毫秒。", userFromPath, DateTimeUtil.getTimeMillis(now));
                        return userFromName;
                    }
                }
            }
        }
        return userFromName;
    }

    /**
     * <B>方法名称：getSegmentDetaiDolList</B>
     * <B>概要说明：组装msSegmentDetailDo实例信息，并放入到list集合中，然后方便下一步的批量处理</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月07日 11:09:24
     * @Param [segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segment, segmentObject]
     **/
    private LinkedList<MsSegmentDetailDo> getSegmentDetaiDolList(LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList, SegmentDo segment, SegmentObject segmentObject) {
        LinkedList<MsSegmentDetailDo> segmentDetaiDolList = new LinkedList<>();
        try {
            String reorganizingSpans = segment.getReorganizingSpans();
            if (StringUtil.isBlank(reorganizingSpans)) {
                putSegmentDetailDoIntoList(segment, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segmentObject);
                return segmentDetaiDolList;
            }
            List<LinkedHashMap> list = JsonUtil.string2Obj(reorganizingSpans, List.class, LinkedHashMap.class);
            if (null == list || list.isEmpty() || 1 == list.size()) {
                putSegmentDetailDoIntoList(segment, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segmentObject);
                return segmentDetaiDolList;
            }
            for (int i = 1; i < list.size(); i++) {
                LinkedHashMap map = list.get(i);
                // 给MsSegmentDetailDo实例赋值
                MsSegmentDetailDo msSegmentDetailDo = getMsSegmentDetailDo(map, segment, list.get(0));

                String operationName = segment.getOperationName();
                // 设置用户来源；2022-11-09 14:49:37
                getUserFrom(operationName, msSegmentDetailDo, segment.getParentEndpoint());

                String logs = String.valueOf(map.get(Const.LOGS));
                Boolean isError = false;

                String tags = String.valueOf(map.get(Const.TAGS));
                List<KeyValue> tagsList = JsonUtil.string2Obj(tags, List.class, KeyValue.class);
                if (null != tagsList) {
                    String isSql = null;
                    String dingTalkContent = null;
                    for (KeyValue keyValue : tagsList) {
                        String key = keyValue.getKey();
                        String value = keyValue.getValue();
                        if (Const.FILE_OUTPUT.equals(key)) {
                            // 给MsSegmentDetailDo实例设置dbType类型和operationType类型
                            mingshiServerUtil.setDbTypeAndOperationType(msSegmentDetailDo, Const.FILE_OUTPUT, Const.FILE_OUTPUT, value);
                        } else if (Const.SEND_EMAIL.equals(key)) {
                            String address = JsonUtil.string2Obj(value, ObjectNode.class).get(Const.ADDREE).asText();
                            msSegmentDetailDo.setPeer(address);
                            mingshiServerUtil.setDbTypeAndOperationType(msSegmentDetailDo, Const.SEND_EMAIL, Const.SEND_EMAIL, value);
                        } else {
                            if (Const.OPERATION_TYPE_DING_TALK.equals(key)) {
                                dingTalkContent = value;
                            } else if (Const.DB_TYPE.equals(key)) {
                                isSql = value;
                                msSegmentDetailDo.setOperationType(value);
                            } else if (Const.DB_INSTANCE.equals(key)) {
                                msSegmentDetailDo.setDbInstance(value);
                            } else if (Const.DB_USER_NAME.equals(key)) {
                                setUserName(msSegmentDetailDo, value);
                            } else if (Const.DB_STATEMENT.equals(key)) {
                                if (Const.OPERATION_TYPE_SQL.equals(isSql)) {
                                    if ((StringUtil.isNotBlank(logs) && !logs.equals(Const.IS_NULL)) || StringUtil.isBlank(value)) {
                                        isError = true;
                                        // 出现了SQL异常，直接退出循环；2022-07-01 14:41:50
                                        break;
                                    }
                                    // 获取表名；2022-06-06 14:16:59
                                    String tableName = setTableName(value, msSegmentDetailDo);
                                    if (StringUtil.isNotBlank(tableName)) {
                                        msSegmentDetailDo.setMsTableName(tableName);
                                    }
                                }
                                msSegmentDetailDo.setDbStatement(value);
                            } else if (key.equals(Const.OPERATION_TYPE_URL)) {
                                if (value.contains(Const.OPERATION_TYPE_DINGTALK)) {
                                    msSegmentDetailDo.setDbType(Const.OPERATION_TYPE_DING_TALK);
                                    ObjectNode jsonObject = JsonUtil.createJsonObject();
                                    jsonObject.put(Const.IP, value);
                                    jsonObject.put(Const.CONTENT, dingTalkContent);
                                    msSegmentDetailDo.setDbStatement(jsonObject.toString());
                                    msSegmentDetailDo.setOperationType(Const.OPERATION_TYPE_DING_TALK);
                                } else {
                                    msSegmentDetailDo.setDbType(key);
                                    msSegmentDetailDo.setDbStatement(value);
                                }
                            }
                        }
                    }
                }

                if (false == isError) {
                    // 当没有出现sql异常时，才保存SQL信息；2022-07-01 14:41:31
                    if (StringUtil.isNotBlank(msSegmentDetailDo.getUserName())) {
                        segmentDetaiDolList.add(msSegmentDetailDo);
                    } else {
                        // 用户名为空，但token和globalTraceId都不为空；2022-08-01 15:26:51
                        if (StringUtil.isNotBlank(msSegmentDetailDo.getGlobalTraceId()) && StringUtil.isNotBlank(msSegmentDetailDo.getToken()) && !msSegmentDetailDo.getDbType().equals(Const.URL)) {
                            segmentDetaiUserNameIsNullDolList.add(msSegmentDetailDo);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.getSegmentDetaiDolList() # 组装segmentDetail详情实例时，出现了异常。", e);
        }
        return segmentDetaiDolList;
    }

    /**
     * <B>方法名称：setUserName</B>
     * <B>概要说明：设置用户名</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-28 17:33:46
     * @Param [msSegmentDetailDo, value]
     **/
    private void setUserName(MsSegmentDetailDo msSegmentDetailDo, String value) {
        msSegmentDetailDo.setDbUserName(value);
        String operationName = msSegmentDetailDo.getOperationName();
        String parentEndpoint = msSegmentDetailDo.getParentEndpoint();
        String userName = msSegmentDetailDo.getUserName();
        // 如果是来自spring框架提供的定时任务，那么用户名是空的。此时把访问数据库的用户名（dbUserName）当做登录系统的用户名（userName）；2022-10-28 17:11:23
        if ((StringUtil.isBlank(userName) && StringUtil.isNotBlank(operationName) && StringUtil.isNotBlank(parentEndpoint)) &&
            (operationName.contains(Const.SPRING_SCHEDULED) || operationName.startsWith(Const.SPRING_SCHEDULED) || parentEndpoint.contains(Const.SPRING_SCHEDULED) || parentEndpoint.startsWith(Const.SPRING_SCHEDULED))) {
            msSegmentDetailDo.setUserName(value);
            msSegmentDetailDo.setUserFrom(Const.USER_FROM_SCHEDULE_TASK);
        }
    }

    /**
     * <B>方法名称：getMsSegmentDetailDo</B>
     * <B>概要说明：给MsSegmentDetailDo实例赋值</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月19日 08:08:49
     * @Param [msSegmentDetailDo, map, segment]
     **/
    private MsSegmentDetailDo getMsSegmentDetailDo(LinkedHashMap map, SegmentDo segment, LinkedHashMap map1) {
        Object url = map1.get(Const.URL);
        MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
        msSegmentDetailDo.setOperationName(String.valueOf(url));

        Integer spanId = null;
        if (null != map.get(Const.SPANID)) {
            spanId = Integer.valueOf(String.valueOf(map.get(Const.SPANID)));
        }
        String component = String.valueOf(map.get(Const.COMPONET));
        String serviceCode = String.valueOf(map.get(Const.SERVICE_CODE));
        String peer = String.valueOf(map.get(Const.PEER));
        msSegmentDetailDo.setPeer(peer);
        String endpointName = String.valueOf(map.get(Const.ENDPOINT_NAME));
        msSegmentDetailDo.setEndpointName(endpointName);
        Long startTime = Long.valueOf(String.valueOf(map.get(Const.START_TIME)));
        String serviceInstanceName = String.valueOf(map.get(Const.SERVICE_INSTANCE_NAME));
        Long endTime = Long.valueOf(String.valueOf(map.get(Const.END_TIME)));
        Integer parentSpanId = Integer.valueOf(String.valueOf(map.get(Const.PARENT_SPAN_ID)));
        msSegmentDetailDo.setToken(segment.getToken());
        msSegmentDetailDo.setComponent(component);
        msSegmentDetailDo.setSpanId(spanId);
        msSegmentDetailDo.setServiceCode(serviceCode);
        msSegmentDetailDo.setStartTime(DateTimeUtil.longToDate(startTime));
        msSegmentDetailDo.setServiceInstanceName(serviceInstanceName);
        msSegmentDetailDo.setEndTime(DateTimeUtil.longToDate(endTime));
        msSegmentDetailDo.setParentSpanId(parentSpanId);
        msSegmentDetailDo.setUserName(segment.getUserName());
        msSegmentDetailDo.setGlobalTraceId(segment.getGlobalTraceId());
        msSegmentDetailDo.setParentSegmentId(segment.getParentSegmentId());
        msSegmentDetailDo.setCurrentSegmentId(segment.getCurrentSegmentId());
        msSegmentDetailDo.setUserLoginIp(segment.getIp());
        msSegmentDetailDo.setParentService(segment.getParentService());
        msSegmentDetailDo.setParentEndpoint(segment.getParentEndpoint());
        msSegmentDetailDo.setParentServiceInstance(segment.getParentServiceInstance());
        return msSegmentDetailDo;
    }

    /**
     * <B>方法名称：putSegmentDetailDoIntoList</B>
     * <B>概要说明：组装MsSegmentDetailDo实例</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月13日 16:09:09
     * @Param [consumerRecord, segment, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segmentObject]
     **/
    private void putSegmentDetailDoIntoList(SegmentDo segment, LinkedList<MsSegmentDetailDo> segmentDetaiDolList, LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList, SegmentObject segmentObject) {
        if (StringUtil.isNotBlank(segment.getUserName()) && (StringUtil.isNotBlank(segment.getToken()) || StringUtil.isNotBlank(segment.getGlobalTraceId()))) {
            MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
            // 设置用户来源；2022-11-09 14:49:37
            getUserFrom(segment.getOperationName(), msSegmentDetailDo, segment.getParentEndpoint());

            msSegmentDetailDo.setParentService(segment.getParentService());
            msSegmentDetailDo.setParentEndpoint(segment.getParentEndpoint());
            msSegmentDetailDo.setParentServiceInstance(segment.getParentServiceInstance());
            msSegmentDetailDo.setUserLoginIp(segment.getIp());
            msSegmentDetailDo.setUserName(segment.getUserName());
            msSegmentDetailDo.setToken(segment.getToken());
            msSegmentDetailDo.setGlobalTraceId(segment.getGlobalTraceId());
            msSegmentDetailDo.setOperationName(segment.getOperationName());
            msSegmentDetailDo.setStartTime(segment.getRequestStartTime());
            msSegmentDetailDo.setEndTime(segment.getRequestStartTime());
            msSegmentDetailDo.setServiceCode(segmentObject.getService());
            msSegmentDetailDo.setCurrentSegmentId(segment.getCurrentSegmentId());
            String reorganizingSpans = segment.getReorganizingSpans();
            List list = JsonUtil.string2Obj(reorganizingSpans, List.class);
            if (null != list && Const.NUMBER_ONE == list.size()) {
                LinkedHashMap hashMap = (LinkedHashMap) list.get(0);
                if (null != hashMap && null != hashMap.get(Const.OPERATION_TYPE_URL)) {
                    msSegmentDetailDo.setOperationType(Const.OPERATION_TYPE_URL_NO_DB_STATEMENT);
                }
            }
            if (StringUtil.isNotBlank(msSegmentDetailDo.getUserName())) {
                // 用户名不为空；
                segmentDetaiDolList.add(msSegmentDetailDo);
            } else {
                // 用户名为空，但token和globalTraceId都不为空；2022-08-01 15:26:51
                if (StringUtil.isNotBlank(msSegmentDetailDo.getGlobalTraceId()) && StringUtil.isNotBlank(msSegmentDetailDo.getToken()) && !msSegmentDetailDo.getDbType().equals(Const.URL)) {
                    segmentDetaiUserNameIsNullDolList.add(msSegmentDetailDo);
                } else {
                    log.error("# SegmentConsumeServiceImpl.putSegmentDetailDoIntoList() # 出现异常了：用户名为空，token或者globalTraceId也为空。【{}】.", JsonUtil.obj2String(segment));
                }
            }

        }
    }

    /**
     * <B>方法名称：setTableName</B>
     * <B>概要说明：获取SQL语句中表的名字</B>
     *
     * @return java.lang.String
     * @Author zm
     * @Date 2022年09月20日 14:09:43
     * @Param [value, msSegmentDetailDo]
     **/
    private String setTableName(String value, MsSegmentDetailDo msSegmentDetailDo) {
        List<String> tableNameList = null;
        String tableName = null;
        try {
            // sql类型；
            String sqlType = mingshiServerUtil.getSqlType(value);
            msSegmentDetailDo.setDbType(sqlType);
            // 获取表名；2022-06-06 14:11:21
            tableNameList = mingshiServerUtil.getTableNameList(sqlType, value);
            if (null != tableNameList && !tableNameList.isEmpty()) {
                for (String tableNameTemp : tableNameList) {
                    String dbInstance = msSegmentDetailDo.getDbInstance();
                    String peer = msSegmentDetailDo.getPeer();
                    String replaceTableName = tableNameTemp.replace("`", "");
                    String key = null;
                    Integer tableEnableStatus = null;
                    if (replaceTableName.contains(Const.EN_COMMA)) {
                        String[] splits = replaceTableName.split(Const.EN_COMMA);
                        for (String splitTableName : splits) {
                            key = mingshiServerUtil.doGetTableName(peer, dbInstance, splitTableName);
                            // 使用数据库地址 + 数据库名称 + 表名，来唯一定位一个表；2022-07-15 10:39:13
                            tableEnableStatus = LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(key);
                            if (null != tableEnableStatus && 1 == tableEnableStatus) {
                                // 如果当前表处于禁用状态，那么直接返回；2022-07-13 11:27:36
                                return null;
                            }
                        }
                    } else {
                        key = mingshiServerUtil.doGetTableName(peer, dbInstance, replaceTableName);
                        // 使用数据库地址 + 数据库名称 + 表名，来唯一定位一个表；2022-07-15 10:39:13
                        tableEnableStatus = LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(key);
                        if (null != tableEnableStatus && 1 == tableEnableStatus) {
                            // 如果当前表处于禁用状态，那么直接返回；2022-07-13 11:27:36
                            return null;
                        }
                    }

                    if (StringUtil.isBlank(tableName)) {
                        tableName = tableNameTemp;
                    } else {
                        tableName = tableName + Const.EN_COMMA + tableNameTemp;
                    }
                }
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.setTableName() # 根据sql语句获取表名时，出现了异常。", e);
        }
        if (StringUtil.isNotBlank(tableName)) {
            tableName = tableName.replace("`", "");
        }
        return tableName;
    }

    /**
     * <B>方法名称：ignoreMethod</B>
     * <B>概要说明：判断是否可以过滤掉</B>
     *
     * @return java.lang.Boolean
     * @Author zm
     * @Date 2022年05月30日 20:05:06
     * @Param [operationName]
     **/
    private Boolean ignoreMethod(String operationName) {
        if (Const.OPERATION_NAME_MAP.containsKey(operationName)) {
            return true;
        }
        return false;
    }

    /**
     * <B>方法名称：statisticsProcessorThreadQps</B>
     * <B>概要说明：组装QPS数据</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月23日 11:07:33
     * @Param [jsonObject]
     **/
    private HashMap<String, Integer> statisticsProcessorThreadQps() {
        HashMap<String, Integer> statisticsProcessorThreadQpsMap = new HashMap<>(Const.NUMBER_EIGHT);
        String key = Const.QPS_ZSET_EVERY_PROCESSOR_THREAD + Thread.currentThread().getName();
        String time = DateTimeUtil.dateToStrformat(new Date());
        if (null == statisticsProcessorThreadQpsMap) {
            statisticsProcessorThreadQpsMap = new HashMap<>(Const.NUMBER_EIGHT);
        }
        Integer accumuCount = statisticsProcessorThreadQpsMap.get(key);
        statisticsProcessorThreadQpsMap.put(time, null == accumuCount ? 1 : 1 + accumuCount);
        return statisticsProcessorThreadQpsMap;
    }

    /**
     * <B>方法名称：getRef</B>
     * <B>概要说明：获取TraceSegmentId</B>
     *
     * @return java.lang.String
     * @Author zm
     * @Date 2022年07月14日 15:07:12
     * @Param [segmentObject, segment]
     **/
    private String getRef(SegmentObject segmentObject, SegmentDo segment) {
        try {
            segment.setServiceCode(segmentObject.getService());
            segment.setServiceInstanceName(segmentObject.getServiceInstance());
            segment.setCurrentSegmentId(segmentObject.getTraceSegmentId());
            segment.setGlobalTraceId(segmentObject.getTraceId());
            String ref = segmentObject.getRef();
            if (!StringUtil.isBlank(ref)) {
                TraceSegmentRef traceSegmentRef = JsonUtil.string2Obj(ref, TraceSegmentRef.class);
                String parentService = traceSegmentRef.getParentService();
                segment.setParentService(parentService);
                String parentServiceInstance = traceSegmentRef.getParentServiceInstance();
                segment.setParentServiceInstance(parentServiceInstance);
                String parentEndpoint = traceSegmentRef.getParentEndpoint();
                segment.setParentEndpoint(parentEndpoint);

                String parentSegmentId = traceSegmentRef.getTraceSegmentId();
                segment.setParentSegmentId(parentSegmentId);
                return parentSegmentId;
            }
        } catch (Exception e) {
            log.error("# getRef() # 根据SegmentObject实例【{}】获取TraceSegmentId时，出现了异常。", segmentObject.toString(), e);
        }
        return null;
    }

    /**
     * <B>方法名称：reorganizingSpans</B>
     * <B>概要说明：重组span数据，返回前端使用</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月07日 11:09:00
     * @Param [segmentDo, spanList]
     **/
    private void reorganizingSpans(SegmentDo segmentDo, List<Span> spanList) {
        if (StringUtil.isBlank(segmentDo.getUserName()) && StringUtil.isBlank(segmentDo.getToken())) {
            return;
        }

        LinkedHashSet<String> ipHashSet = new LinkedHashSet<>();
        List<String> linkedList = new LinkedList<>();
        if (CollectionUtils.isNotEmpty(spanList)) {
            List<Span> rootSpans = findRoot(spanList);
            for (Span span : rootSpans) {
                List<Span> childrenSpan = new ArrayList<>();
                childrenSpan.add(span);

                // 获取用户登录的ip；2022-09-07 11:17:09
                getIps(ipHashSet, span);

                // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
                getData(segmentDo, span, linkedList);
                findChildrenDetail(segmentDo, spanList, span, childrenSpan, linkedList);
            }
        }
        if (!ipHashSet.isEmpty()) {
            segmentDo.setIp(JsonUtil.obj2String(ipHashSet));
        }
        String toString = linkedList.toString();
        segmentDo.setReorganizingSpans(toString);
    }

    /**
     * <B>方法名称：getIps</B>
     * <B>概要说明：获取用户登录的ip</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月07日 11:09:26
     * @Param [ipHashSet, span]
     **/
    private void getIps(LinkedHashSet<String> ipHashSet, Span span) {
        String ips = span.getIp();
        if (StringUtil.isNotBlank(ips)) {
            if (ips.contains(Const.EN_COMMA)) {
                String[] splits = ips.split(Const.EN_COMMA);
                for (String ip : splits) {
                    ipHashSet.add(ip);
                }
            } else {
                ipHashSet.add(ips);
            }
        }
    }

    private List<Span> findRoot(List<Span> spans) {
        List<Span> rootSpans = new ArrayList<>();
        spans.forEach(span -> {
            String segmentParentSpanId = span.getSegmentParentSpanId();

            boolean hasParent = false;
            for (Span subSpan : spans) {
                if (segmentParentSpanId.equals(subSpan.getSegmentSpanId())) {
                    hasParent = true;
                    // if find parent, quick exit
                    break;
                }
            }

            if (!hasParent) {
                span.setRoot(true);
                rootSpans.add(span);
            }
        });
        rootSpans.sort(Comparator.comparing(Span::getStartTime));
        return rootSpans;
    }

    private void findChildrenDetail(SegmentDo segmentDo, List<Span> spans, Span parentSpan, List<Span> childrenSpan, List<String> linkedList) {
        spans.forEach(span -> {
            if (span.getSegmentParentSpanId().equals(parentSpan.getSegmentSpanId())) {
                childrenSpan.add(span);
                getData(segmentDo, span, linkedList);
                findChildrenDetail(segmentDo, spans, span, childrenSpan, linkedList);
            }
        });
    }

    /**
     * <B>方法名称：getData</B>
     * <B>概要说明：只要span中的tags字段不为空，那么就把这个span放入到链表中。这样做的目的是：不再区分是哪个插件拦截到的信息，像skywalking的服务端一样，使用统一的展示方式在前端展示数据。</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年05月17日 10:05:41
     * @Param [span, linkedList]
     **/
    private void getData(SegmentDo segmentDo, Span span, List<String> linkedList) {
        try {
            ObjectNode jsonObject = JsonUtil.createJsonObject();
            int spanId = span.getSpanId();
            if (0 == spanId) {
                getSpringMvcInfo(span, jsonObject, linkedList);
            } else if (!span.getTags().isEmpty()) {
                jsonObject.put(Const.SPANID, spanId);
                jsonObject.put(Const.PARENT_SPAN_ID, span.getParentSpanId());
                jsonObject.put(Const.SERVICE_CODE, span.getServiceCode());
                jsonObject.put(Const.SERVICE_INSTANCE_NAME, span.getServiceInstanceName());
                jsonObject.put(Const.START_TIME, span.getStartTime());
                jsonObject.put(Const.END_TIME, span.getEndTime());
                jsonObject.put(Const.ENDPOINT_NAME, span.getEndpointName());
                jsonObject.put(Const.PEER, span.getPeer());
                jsonObject.put(Const.COMPONET, span.getComponent());
                List<LogEntity> logs = span.getLogs();
                if (null != logs && !logs.isEmpty()) {
                    jsonObject.put(Const.LOGS, JsonUtil.obj2String(logs));
                }
                List<KeyValue> tags = span.getTags();
                if (!tags.isEmpty()) {
                    Boolean flag = false;
                    String key = null;
                    String url = null;
                    String httpBody = null;
                    for (KeyValue tag : tags) {
                        key = tag.getKey();
                        if (segmentDo.getOperationName().equals(Const.JEDIS_SENTINEL_GET_MASTER_ADDR_BY_NAME)) {
                            flag = true;
                            break;
                        }
                        if (tag.getValue().equals(Const.REDIS)) {
                            // 不再存储单纯的Redis请求；2022-06-30 16:34:24
                            flag = true;
                            break;
                        }
                        if (key.equals(Const.HTTP_BODY)) {
                            // 不再存储单纯的GET请求；2022-05-27 18:14:25
                            httpBody = tag.getValue();
                        } else if (key.equals(Const.URL)) {
                            // 不再存储单纯的GET请求；2022-05-27 18:14:25
                            url = tag.getValue();
                        }
                    }

                    getUserNameFromHttpBody(segmentDo, url, httpBody);
                    if (false == flag) {
                        jsonObject.put(Const.TAGS, JsonUtil.obj2String(tags));
                        linkedList.add(jsonObject.toString());
                    }
                }
            }
        } catch (Exception e) {
            log.error("将span的信息 = 【{}】放入到LinkedList中的时候，出现了异常。", JsonUtil.obj2StringPretty(span), e);
        }
    }

    /**
     * <B>方法名称：getUserNameFromHttpBody</B>
     * <B>概要说明：从Http的返回body中获取用户名</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月11日 17:07:09
     * @Param [segmentDo, url, httpBody]
     **/
    private void getUserNameFromHttpBody(SegmentDo segmentDo, String url, String httpBody) {
        try {
            // 有一个特殊的url（http://172.17.80.184:8181/login/fish/easier），其用户名放在了HTTP返回的body中。
            // 这个url之所以特殊，是因为用户在浙里办APP上进入渔省心，然后在渔省心里获取用户名信息，接着做其他的操作。
            // 2022-07-11 17:28:27
            if (StringUtil.isNotBlank(url) && url.contains(Const.LOGIN_FISH_EASIER)) {
                CommonResponse commonResponse = JsonUtil.string2Obj(httpBody, CommonResponse.class);
                if (null != commonResponse) {
                    Object data = commonResponse.getData();
                    if (null != data) {
                        String username = StringUtil.isBlank(String.valueOf(((LinkedHashMap) data).get(Const.NICKNAME))) == true ? String.valueOf(((LinkedHashMap) data).get(Const.USERNAME)) : String.valueOf(((LinkedHashMap) data).get(Const.NICKNAME));
                        if (StringUtil.isNotBlank(username) && StringUtil.isBlank(segmentDo.getUserName())) {
                            segmentDo.setUserName(username);
                            segmentDo.setUserFrom(Const.USER_FROM_ZHE_LI_BAN);
                            setUserNameTokenGlobalTraceIdToLocalMemory(segmentDo);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.getUserNameFromHttpBody() # 从调用的url = 【{}】中获取用户名时，出现了异常。", url, e);
        }
    }

    /**
     * <B>方法名称：getSpringMVCInfo</B>
     * <B>概要说明：获取SpringMVC信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年05月12日 16:05:24
     * @Param [span, jsonObject, linkedList]
     **/
    private void getSpringMvcInfo(Span span, ObjectNode jsonObject, List<String> linkedList) {
        List<KeyValue> tagsList = span.getTags();
        for (KeyValue keyValue : tagsList) {
            if (keyValue.getKey().equals(Const.URL)) {
                String url = keyValue.getValue();
                jsonObject.put(Const.URL, url);
                linkedList.add(jsonObject.toString());
            }
        }
    }

    /**
     * <B>方法名称：setUserNameTokenGlobalTraceIdToLocalMemory </B>
     * <B>概要说明：将userName、token，与globalTraceId关联起来 </B>
     * 注：userName与token是等价的。当用户第一次登录时，如果用户校验成功，那么下次用户再访问其他接口时，会使用token来代替用户名。
     *
     * @return void
     * @Author zm
     * @Date 2022年05月23日 10:05:22
     * @Param [segment]
     **/
    private void setUserNameTokenGlobalTraceIdToLocalMemory(SegmentDo segment) {
        String globalTraceId = segment.getGlobalTraceId();
        // 用户名和token都是空的调用链，不存入数据库中。这里只存入带有用户名或者token完整的调用链。2022-04-20 16:35:52
        String segmentUserName = segment.getUserName();
        String segmentToken = segment.getToken();
        String userName = null;
        if (StringUtil.isBlank(segmentUserName) && StringUtil.isBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
            // 当用户名和token都为null，但全局追踪id不为空；
            // 首先根据globalTraceId获取userName；
            userName = MsCaffeine.getUserNameByGlobalTraceId(globalTraceId);
            if (StringUtil.isNotBlank(userName) && StringUtil.isBlank(segment.getUserName())) {
                segment.setUserName(userName);
            }

            // 首先根据globalTraceId获取token；
            String token = MsCaffeine.getTokenByGlobalTraceId(globalTraceId);
            if (StringUtil.isNotBlank(token)) {
                // 首先根据 token 获取 userName；
                userName = MsCaffeine.getUserNameByToken(token);
                if (StringUtil.isNotBlank(userName) && StringUtil.isBlank(segment.getUserName())) {
                    segment.setUserName(userName);
                    MsCaffeine.putUserNameByGlobalTraceId(globalTraceId, userName);
                }
            }
        } else if (StringUtil.isBlank(segmentUserName) && StringUtil.isNotBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
            MsCaffeine.putTokenByGlobalTraceId(globalTraceId, segmentToken);
            // 当用户名为null，但token和全局追踪id不为空；
            userName = MsCaffeine.getUserNameByGlobalTraceId(globalTraceId);
            if (StringUtil.isNotBlank(userName) && StringUtil.isBlank(segment.getUserName())) {
                MsCaffeine.putUserNameByToken(segmentToken, userName);
                segment.setUserName(userName);
            } else {
                userName = MsCaffeine.getUserNameByToken(segmentToken);
                if (StringUtil.isNotBlank(userName) && StringUtil.isBlank(segment.getUserName())) {
                    segment.setUserName(userName);
                    MsCaffeine.putUserNameByGlobalTraceId(globalTraceId, userName);
                }
            }
        } else if (StringUtil.isNotBlank(segmentUserName) && StringUtil.isNotBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
            // 当用户名、token和全局追踪id都不为空；这时候，就可以把三个map补全了。2022-05-24 15:48:15
            MsCaffeine.putUserNameByGlobalTraceId(globalTraceId, segmentUserName);
            MsCaffeine.putTokenByGlobalTraceId(globalTraceId, segmentToken);
            MsCaffeine.putUserNameByToken(segmentToken, segmentUserName);
        } else if (StringUtil.isNotBlank(segmentUserName) && StringUtil.isBlank(segmentToken) && StringUtil.isNotBlank(globalTraceId)) {
            MsCaffeine.putUserNameByGlobalTraceId(globalTraceId, segmentUserName);
        } else {
            log.error("# SegmentConsumeServiceImpl.setUserNameTokenGlobalTraceIdToLocalMemory() # 出现异常情况了。用户名、token和全局追踪id都为空。");
        }
    }

    private List<Span> buildSpanList(SegmentObject segmentObject, SegmentDo segmentDo) {
        List<Span> spans = new ArrayList<>();

        List<SpanObject> spansList = segmentObject.getSpansList();
        if (null != spansList && !spansList.isEmpty()) {
            segmentDo.setSpans(spansList.toString());
        }
        for (SpanObject spanObject : spansList) {
            String operationName = spanObject.getOperationName();

            // 忽略掉不需要的链路信息；2022-06-28 14:18:20
            Boolean flag = ignoreMethod(operationName);
            if (true == flag) {
                continue;
            }

            // 构建span；2022-09-20 14:30:47
            doBuildSpan(segmentObject, spanObject, spans);
        }

        return spans;
    }

    /**
     * <B>方法名称：doBuildSpan</B>
     * <B>概要说明：构建span</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月20日 14:09:43
     * @Param [segmentObject, spanObject, spans]
     **/
    private void doBuildSpan(SegmentObject segmentObject, SpanObject spanObject, List<Span> spans) {
        String operationName = spanObject.getOperationName();
        Span span = new Span();
        span.setUserName(spanObject.getUserName());
        span.setToken(spanObject.getToken());

        span.setTraceId(segmentObject.getTraceId());
        span.setSegmentId(segmentObject.getTraceSegmentId());
        span.setSpanId(spanObject.getSpanId());
        span.setParentSpanId(spanObject.getParentSpanId());
        span.setStartTime(spanObject.getStartTime());
        span.setEndTime(spanObject.getEndTime());
        span.setError(spanObject.getIsError());
        span.setLayer(spanObject.getSpanLayer().name());
        span.setType(spanObject.getSpanType().name());

        String segmentSpanId = segmentObject.getTraceSegmentId() + Const.SEGMENT_SPAN_SPLIT + spanObject.getSpanId();
        span.setSegmentSpanId(segmentSpanId);

        String segmentParentSpanId = segmentObject.getTraceSegmentId() + Const.SEGMENT_SPAN_SPLIT + spanObject.getParentSpanId();
        span.setSegmentParentSpanId(segmentParentSpanId);

        span.setPeer(spanObject.getPeer());

        span.setEndpointName(operationName);

        span.setServiceCode(segmentObject.getService());
        span.setServiceInstanceName(segmentObject.getServiceInstance());

        String component = ComponentsDefine.getComponentMap().get(spanObject.getComponentId());
        if (!StringUtil.isBlank(component)) {
            span.setComponent(component);
        }

        spanObject.getRefsList().forEach(reference -> {
            Ref ref = new Ref();
            ref.setTraceId(reference.getTraceId());
            ref.setParentSegmentId(reference.getParentTraceSegmentId());

            switch (reference.getRefType()) {
                case CrossThread:
                    ref.setType(RefType.CROSS_THREAD);
                    break;
                case CrossProcess:
                    ref.setType(RefType.CROSS_PROCESS);
                    break;
                default:
                    break;
            }
            ref.setParentSpanId(reference.getParentSpanId());

            span.setSegmentParentSpanId(
                ref.getParentSegmentId() + Const.SEGMENT_SPAN_SPLIT + ref.getParentSpanId());

            span.getRefs().add(ref);
        });

        spanObject.getTagsList().forEach(tag -> {
            KeyValue keyValue = new KeyValue();
            keyValue.setKey(tag.getKey());
            keyValue.setValue(tag.getValue());
            span.getTags().add(keyValue);
        });

        spanObject.getLogsList().forEach(log -> {
            LogEntity logEntity = new LogEntity();
            logEntity.setTime(log.getTime());

            log.getDataList().forEach(data -> {
                KeyValue keyValue = new KeyValue();
                keyValue.setKey(data.getKey());
                keyValue.setValue(data.getValue());
                logEntity.getData().add(keyValue);
            });

            span.getLogs().add(logEntity);
        });

        spans.add(span);
    }

    /**
     * <B>方法名称：setUserNameAndTokenFromSpan</B>
     * <B>概要说明：从span中获取用户名和token</B>
     *
     * @return com.aiit.skyflying.common.domain.SegmentDo
     * @Author zm
     * @Date 2022年07月12日 10:07:33
     * @Param [spanList, segment]
     **/
    private SegmentDo setUserNameAndTokenFromSpan(List<Span> spanList, SegmentDo segment) {
        try {
            if (null == segment) {
                segment = new SegmentDo();
            }
            if (null == spanList && !spanList.isEmpty()) {
                return segment;
            }
            Span span = spanList.get(spanList.size() - 1);
            segment.setOperationName(span.getEndpointName());
            segment.setRequestStartTime(DateTimeUtil.longToDate(span.getStartTime()));

            if (StringUtil.isNotBlank(segment.getUserName()) && StringUtil.isNotBlank(segment.getToken())) {
                return segment;
            }
            String userName = span.getUserName();
            String token = span.getToken();
            if (StringUtil.isNotBlank(userName) && StringUtil.isBlank(segment.getUserName())) {
                segment.setUserName(userName);
            }
            if (StringUtil.isNotBlank(token)) {
                segment.setToken(token);
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.setUserNameAndTokenFromSpan() # 从Span实例中获取用户名和token时，出现了异常。", e);
        }
        return segment;
    }

    /**
     * <B>方法名称：setUserNameAndTokenAndIpFromSegmentObject</B>
     * <B>概要说明：从SegmentObject中获取用户名、token和ip</B>
     *
     * @return com.aiit.skyflying.common.domain.SegmentDo
     * @Author zm
     * @Date 2022年07月12日 10:07:59
     * @Param [spanList, segment]
     **/
    private SegmentDo setUserNameAndTokenAndIpFromSegmentObject(HashSet<String> userHashSet, SegmentDo segment, SegmentObject segmentObject) {
        try {
            if (null == segment) {
                segment = new SegmentDo();
            }

            // 获取用户名和token；2022-07-12 10:04:05
            String userName = segmentObject.getUserName();
            if (StringUtil.isNotBlank(userName) && StringUtil.isBlank(segment.getUserName())) {
                segment.setUserName(userName);
            }
            if (StringUtil.isNotBlank(userName)) {
                Boolean userIsExisted = InformationOverviewSingleton.userIsExisted(userName);
                if (!userIsExisted) {
                    userHashSet.add(userName);
                }
            }

            String token = segmentObject.getToken();
            if (StringUtil.isNotBlank(token)) {
                segment.setToken(token);
            }

            String ip = segmentObject.getIp();
            if (StringUtil.isNotBlank(ip) && StringUtil.isBlank(segment.getIp())) {
                segment.setIp(ip);
            }
        } catch (Exception e) {
            log.error("# SegmentConsumeServiceImpl.setUserNameAndTokenFromSegmentObject() # 从SegmentObject实例中获取用户名和token时，出现了异常。", e);
        }
        return segment;
    }
}

