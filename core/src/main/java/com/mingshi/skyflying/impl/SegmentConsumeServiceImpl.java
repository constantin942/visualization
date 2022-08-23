package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.protobuf.InvalidProtocolBufferException;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionUtil;
import com.mingshi.skyflying.caffeine.MsCaffeine;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.elasticsearch.domain.EsMsSegmentDetailDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.type.KeyValue;
import com.mingshi.skyflying.common.type.LogEntity;
import com.mingshi.skyflying.common.type.RefType;
import com.mingshi.skyflying.common.utils.CollectionUtils;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.component.ComponentsDefine;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.disruptor.processor.SegmentByByte;
import com.mingshi.skyflying.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.service.SegmentConsumerService;
import com.mingshi.skyflying.statistics.InformationOverviewSingleton;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.apache.skywalking.apm.network.language.agent.v3.SpanObject;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;

/**
 * <B>方法名称：SegmentConsumeServiceImpl</B>
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
  private MingshiServerUtil mingshiServerUtil;
  @Resource
  private SegmentDao segmentDao;

  @Override
  public ServerResponse<String> consume(ConsumerRecord<String, Bytes> record, Boolean enableReactorModelFlag) {
    SegmentObject segmentObject = null;
    try {
      segmentObject = SegmentObject.parseFrom(record.value().get());
      doConsume(segmentObject, enableReactorModelFlag);
    } catch (InvalidProtocolBufferException e) {
      log.error("# consume() # 消费skywalking探针发送来的数据时，出现了异常。", e);
    }
    return null;
  }

  @Override
  public ServerResponse<String> consumeByDisruptor(SegmentByByte record, Boolean enableReactorModelFlag) {
    SegmentObject segmentObject = null;
    try {
      if (null != record) {
        segmentObject = SegmentObject.parseFrom(record.getData());
        doConsume(segmentObject, enableReactorModelFlag);
      }
    } catch (InvalidProtocolBufferException e) {
      log.error("# consume() # 消费skywalking探针发送来的数据时，出现了异常。", e);
    }
    return null;
  }

  private void doConsume(SegmentObject segmentObject, Boolean enableReactorModelFlag) {
    HashSet<String> userHashSet = new HashSet<>();
    Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
    try {
      SegmentDo segment = new SegmentDo();
      // 设置segment_id、trace_id；2022-04-24 14:26:12
      getRef(segmentObject, segment);
      // 从SegmentObject实例中获取用户名和token；2022-07-12 10:22:53
      setUserNameAndTokenFromSegmentObject(userHashSet, segment, segmentObject);
      // 将用户名、token、globalTraceId放入到本地内存，并关联起来；2022-07-07 16:15:53
      setUserNameTokenGlobalTraceIdToLocalMemory(segment);

      // 判断是否是异常信息；2022-06-07 18:00:13
      LinkedList<MsAlarmInformationDo> msAlarmInformationDoList = null;
      // 将一条访问操作过程中涉及到的多条SQL语句拆成一条一条的SQL；2022-06-09 08:55:18
      LinkedList<EsMsSegmentDetailDo> esSegmentDetaiDolList = null;
      LinkedList<MsSegmentDetailDo> segmentDetaiDolList = null;
      LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList = null;
      // 获取探针的名称；2022-06-28 14:25:46
      skywalkingAgentHeartBeatMap = getAgentServiceName(segmentObject);

      List<Span> spanList = buildSpanList(segmentObject, segment);

      if (null != spanList && 0 < spanList.size()) {
        // 组装segment；2022-04-20 16:33:48
        segment = setUserNameAndTokenFromSpan(spanList, segment);

        // 重组span数据，返回前端使用；2022-04-20 16:49:02
        reorganizingSpans(segment, spanList);

        segmentDetaiDolList = new LinkedList<>();
        segmentDetaiUserNameIsNullDolList = new LinkedList<>();
        getSegmentDetaiDolList(segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segment, segmentObject);

        // 判断是否是异常信息；2022-06-07 18:00:13
        msAlarmInformationDoList = new LinkedList<>();

        AnomalyDetectionUtil.userVisitedTimeIsAbnormal(segment, msAlarmInformationDoList);
        AnomalyDetectionUtil.userVisitedTableIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
      }

      HashMap<String, Map<String, Integer>> statisticsProcessorThreadQpsMap = new HashMap<>(Const.NUMBER_EIGHT);
      statisticsProcessorThreadQps(statisticsProcessorThreadQpsMap);

      // 将组装好的segment插入到表中；2022-04-20 16:34:01
      if (true == enableReactorModelFlag) {
        // 使用reactor模型；2022-05-30 21:04:05
        mingshiServerUtil.doEnableReactorModel(statisticsProcessorThreadQpsMap, spanList, esSegmentDetaiDolList, segment, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, msAlarmInformationDoList, skywalkingAgentHeartBeatMap);
      } else {
        disableReactorModel(statisticsProcessorThreadQpsMap, userHashSet, skywalkingAgentHeartBeatMap, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, msAlarmInformationDoList);
      }
    } catch (Exception e) {
      log.error("清洗调用链信息时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：disableReactorModel</B>
   * <B>概要说明：不使用Reactor模型</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月19日 18:08:34
   * @Param [statisticsProcessorThreadQpsMap, userHashSet, skywalkingAgentHeartBeatMap, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, msAlarmInformationDoList]
   **/
  private void disableReactorModel(HashMap<String, Map<String, Integer>> statisticsProcessorThreadQpsMap, HashSet<String> userHashSet, Map<String, String> skywalkingAgentHeartBeatMap, LinkedList<MsSegmentDetailDo> segmentDetaiDolList, LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList, LinkedList<MsAlarmInformationDo> msAlarmInformationDoList) {
    // 将QPS信息刷入Redis中；2022-06-27 13:42:13
    // mingshiServerUtil.flushQpsToRedis();

    // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
    mingshiServerUtil.flushSkywalkingAgentInformationToDb();

    // 统计processor线程的QPS；2022-07-23 11:26:40
    mingshiServerUtil.flushProcessorThreadQpsToRedis(statisticsProcessorThreadQpsMap);

    // mingshiServerUtil.flushSpansToDB(spanList);

    mingshiServerUtil.flushUserNameToRedis(userHashSet);

    // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
    mingshiServerUtil.flushSkywalkingAgentInformationToDb();

    // 将探针名称发送到Redis中，用于心跳检测；2022-06-27 13:42:13
    mingshiServerUtil.flushSkywalkingAgentNameToRedis(skywalkingAgentHeartBeatMap);

    // 插入segment数据；2022-05-23 10:15:22
    // LinkedList<SegmentDo> segmentDoLinkedList = new LinkedList<>();
    // if (null != segment) {
    //   segmentDoLinkedList.add(segment);
    //   mingshiServerUtil.flushSegmentToDB(segmentDoLinkedList);
    // }

    // 将表名字插入到监管表中；2022-07-13 14:16:57
    mingshiServerUtil.insertMonitorTables();

    // 将segmentDetail实例信息插入到数据库中；2022-06-02 11:07:51
    // if (true == esMsSegmentDetailUtil.getEsEnable()) {
    //   mingshiServerUtil.flushSegmentDetailToEs(esSegmentDetaiDolList);
    // }

    mingshiServerUtil.flushSegmentDetailToDb(segmentDetaiDolList);

    mingshiServerUtil.flushSegmentDetailUserNameIsNullToDb(segmentDetaiUserNameIsNullDolList);

    // 将异常信息插入到MySQL中；2022-06-07 18:16:44
    LinkedList<MsAlarmInformationDo> msAlarmInformationDoLinkedListist = new LinkedList<>();
    if (null != msAlarmInformationDoList && 0 < msAlarmInformationDoList.size()) {
      msAlarmInformationDoLinkedListist.addAll(msAlarmInformationDoList);
    }
    mingshiServerUtil.flushAbnormalToDb(msAlarmInformationDoLinkedListist);
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
      jsonObject.put("serviceCode", service);
      jsonObject.put("serviceInstanceName", serviceInstance);
      // todo：不应该用当前时间，如果当kafka中出现了消息积压时，那么这个时间就不是探针目前存活的时间。这个时间应该用消息本身的时间。2022-07-04 10:03:17
      skywalkingAgentHeartBeatMap.put(jsonObject.toString(), DateTimeUtil.DateToStr(new Date()));
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.getAgentServiceName() # 获取探针的名称时，出现了异常。", e);
    }
    return skywalkingAgentHeartBeatMap;
  }

  private void getSegmentDetaiDolList(LinkedList<MsSegmentDetailDo> segmentDetaiDolList, LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList, SegmentDo segment, SegmentObject segmentObject) {
    try {
      String reorganizingSpans = segment.getReorganizingSpans();
      if (StringUtil.isBlank(reorganizingSpans)) {
        putSegmentDetailDoIntoList(segment, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segmentObject);
        return;
      }
      List<LinkedHashMap> list = JsonUtil.string2Obj(reorganizingSpans, List.class, LinkedHashMap.class);
      if (null == list || 0 == list.size() || 1 == list.size()) {
        putSegmentDetailDoIntoList(segment, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, segmentObject);
        return;
      }
      MsSegmentDetailDo msSegmentDetailDo = null;
      for (int i = 1; i < list.size(); i++) {
        LinkedHashMap map = list.get(i);

        // 给MsSegmentDetailDo实例赋值
        msSegmentDetailDo = getMsSegmentDetailDo(map, segment, list.get(0));
        String logs = String.valueOf(map.get("logs"));
        Boolean isError = false;

        String tags = String.valueOf(map.get("tags"));
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
                msSegmentDetailDo.setDbUserName(value);
              } else if (Const.DB_STATEMENT.equals(key)) {
                if (Const.OPERATION_TYPE_SQL.equals(isSql)) {
                  if ((StringUtil.isNotBlank(logs) && !logs.equals(Const.IS_NULL)) || StringUtil.isBlank(value)) {
                    isError = true;
                    // 出现了SQL异常，直接退出循环；2022-07-01 14:41:50
                    break;
                  }
                  // 获取表名；2022-06-06 14:16:59
                  String tableName = setTableName(value, msSegmentDetailDo);
                  if (StringUtil.isBlank(tableName)) {
                    isError = true;
                    // 出现了SQL异常，直接退出循环；2022-07-01 14:41:50
                    break;
                  }
                  msSegmentDetailDo.setMsTableName(tableName);
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
            if (StringUtil.isNotBlank(msSegmentDetailDo.getGlobalTraceId()) && StringUtil.isNotBlank(msSegmentDetailDo.getToken())) {
              segmentDetaiUserNameIsNullDolList.add(msSegmentDetailDo);
            } else {
              log.error("# SegmentConsumeServiceImpl.getSegmentDetaiDolList() # 出现异常了：用户名为空，token或者globalTraceId也为空。【{}】.", JsonUtil.obj2String(segment));
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.getSegmentDetaiDolList() # 组装segmentDetail详情实例时，出现了异常。", e);
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
    Object url = map1.get("url");
    MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
    msSegmentDetailDo.setUserPortraitFlagByVisitedTime(null == segment.getUserPortraitFlagByVisitedTime() ? 0 : segment.getUserPortraitFlagByVisitedTime());
    msSegmentDetailDo.setOperationName(String.valueOf(url));

    Integer spanId = null;
    if (null != map.get("spanId")) {
      spanId = Integer.valueOf(String.valueOf(map.get("spanId")));
    }
    String component = String.valueOf(map.get("component"));
    String serviceCode = String.valueOf(map.get("serviceCode"));
    String peer = String.valueOf(map.get("peer"));
    msSegmentDetailDo.setPeer(peer);
    String endpointName = String.valueOf(map.get("endpointName"));
    msSegmentDetailDo.setEndpointName(endpointName);
    Long startTime = Long.valueOf(String.valueOf(map.get(Const.START_TIME)));
    String serviceInstanceName = String.valueOf(map.get("serviceInstanceName"));
    Long endTime = Long.valueOf(String.valueOf(map.get(Const.END_TIME)));
    Integer parentSpanId = Integer.valueOf(String.valueOf(map.get("parentSpanId")));
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
    return msSegmentDetailDo;
  }

  private void putSegmentDetailDoIntoList(SegmentDo segment, LinkedList<MsSegmentDetailDo> segmentDetaiDolList, LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList, SegmentObject segmentObject) {
    if (StringUtil.isNotBlank(segment.getUserName()) && (StringUtil.isNotBlank(segment.getToken()) || StringUtil.isNotBlank(segment.getGlobalTraceId()))) {
      MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
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
      if (null != list && 1 == list.size()) {
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
        if (StringUtil.isNotBlank(msSegmentDetailDo.getGlobalTraceId()) && StringUtil.isNotBlank(msSegmentDetailDo.getToken())) {
          segmentDetaiUserNameIsNullDolList.add(msSegmentDetailDo);
        } else {
          log.error("# SegmentConsumeServiceImpl.putSegmentDetailDoIntoList() # 出现异常了：用户名为空，token或者globalTraceId也为空。【{}】.", JsonUtil.obj2String(segment));
        }
      }

    }
  }

  private String setTableName(String value, MsSegmentDetailDo msSegmentDetailDo) {
    List<String> tableNameList = null;
    String tableName = null;
    try {
      // sql类型；
      String sqlType = mingshiServerUtil.getSqlType(value);
      msSegmentDetailDo.setDbType(sqlType);
      // 获取表名；2022-06-06 14:11:21
      tableNameList = mingshiServerUtil.getTableNameList(sqlType, value);
      for (String tableNameTemp : tableNameList) {
        String dbInstance = msSegmentDetailDo.getDbInstance();
        String peer = msSegmentDetailDo.getPeer();
        String replaceTableName = tableNameTemp.replace("`", "");
        String key = null;
        Integer tableEnableStatus = null;
        if (replaceTableName.contains(",")) {
          String[] splits = replaceTableName.split(",");
          for (String splitTableName : splits) {
            key = mingshiServerUtil.doGetTableName(peer, dbInstance, splitTableName);
            // 使用数据库地址 + 数据库名称 + 表名，来唯一定位一个表；2022-07-15 10:39:13
            tableEnableStatus = LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(key, false);
            if (null != tableEnableStatus && 1 == tableEnableStatus) {
              // 如果当前表处于禁用状态，那么直接返回；2022-07-13 11:27:36
              return null;
            }
          }
        } else {
          key = mingshiServerUtil.doGetTableName(peer, dbInstance, replaceTableName);
          // 使用数据库地址 + 数据库名称 + 表名，来唯一定位一个表；2022-07-15 10:39:13
          tableEnableStatus = LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(key, false);
          if (null != tableEnableStatus && 1 == tableEnableStatus) {
            // 如果当前表处于禁用状态，那么直接返回；2022-07-13 11:27:36
            return null;
          }
        }
        if (StringUtil.isBlank(tableName)) {
          tableName = tableNameTemp;
        } else {
          tableName = tableName + "," + tableNameTemp;
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
    // TODO: 2022/6/28 正常来说，需要忽略的操作类型应该配置到数据库中或者配置到配置文件中，不应该写死在这里。
    if (operationName.equals("Redisson/PING") ||
      operationName.equals("Jedis/sentinelGetMasterAddrByName") ||
      operationName.equals("Lettuce/SENTINEL") ||
      operationName.equals("Mysql/JDBI/Connection/close") ||
      operationName.equals("GET:/devices/notification") ||
      operationName.equals("GET:/manager/html") ||
      operationName.equals("Balancer/user/checkToken") ||
      operationName.equals("GET:") ||
      operationName.equals("GET:/assets/fonts/Nunito-Bold.woff2") ||
      operationName.equals("GET:/temp/null") ||
      operationName.equals("GET:/assets/fonts/Nioicon.ttf") ||
      operationName.equals("GET:/assets/fonts/Nunito-Regular.woff2") ||
      operationName.equals("GET:/assets/fonts/Roboto-Regular.woff2") ||
      operationName.equals("GET:/assets/fonts/Roboto-Medium.woff2") ||
      operationName.equals("HikariCP/Connection/getConnection") ||
      operationName.equals("GET:/") ||
      operationName.equals("GET:/companies/companyHealth/list") ||
      operationName.equals("null:null") ||
      operationName.equals("Mysql/JDBI/PreparedStatement/executeUpdate") ||
      operationName.equals("HikariCP/Connection/close") ||
      operationName.equals("POST:/users/menusAuths") ||
      operationName.equals("GET:/zlb/getRuralCommercialBank/info") ||
      operationName.equals("Mysql/JDBI/Connection/commit") ||
      operationName.equals("Mysql/JDBI/PreparedStatement/executeUpdate") ||
      operationName.equals("HikariCP/Connection/close") ||
      operationName.equals("Mysql/JDBI/PreparedStatement/executeQuery") ||
      operationName.startsWith("SpringScheduled") ||
      operationName.equals("POST:/devices/heartbeat")) {
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
  private void statisticsProcessorThreadQps(HashMap<String, Map<String, Integer>> map) {
    HashMap<String, Integer> hashMap = new HashMap<>(Const.NUMBER_EIGHT);
    hashMap.put(DateTimeUtil.dateToStrformat(new Date()), 1);
    map.put(Const.QPS_ZSET_EVERY_PROCESSOR_THREAD + Thread.currentThread().getName(), hashMap);
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
        String parentSegmentId = traceSegmentRef.getTraceSegmentId();
        segment.setParentSegmentId(parentSegmentId);
        return parentSegmentId;
      }
    } catch (Exception e) {
      log.error("# getRef() # 根据SegmentObject实例【{}】获取TraceSegmentId时，出现了异常。", segmentObject.toString(), e);
    }
    return null;
  }

  private void reorganizingSpans(SegmentDo segment, List<Span> spanList) {
    if (StringUtil.isBlank(segment.getUserName()) && StringUtil.isBlank(segment.getToken())) {
      // log.error("开始执行 AiitKafkaConsumer # reorganizingSpans()方法，该调用链 = 【{}】 不含有用户名或者token，不能插入到表中。", JsonUtil.obj2String(segment));
      return;
    }

    List<String> linkedList = new LinkedList<>();
    if (CollectionUtils.isNotEmpty(spanList)) {
      List<Span> rootSpans = findRoot(spanList);
      for (Span span : rootSpans) {
        List<Span> childrenSpan = new ArrayList<>();
        childrenSpan.add(span);

        // 在这个方法里面组装前端需要的数据；2022-04-14 14:35:37
        getData2(segment, span, linkedList);
        findChildrenDetail(segment, spanList, span, childrenSpan, linkedList);
      }
    }

    String toString = linkedList.toString();
    segment.setReorganizingSpans(toString);
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
        getData2(segmentDo, span, linkedList);
        findChildrenDetail(segmentDo, spans, span, childrenSpan, linkedList);
      }
    });
  }

  /**
   * <B>方法名称：getData2</B>
   * <B>概要说明：只要span中的tags字段不为空，那么就把这个span放入到链表中。这样做的目的是：不再区分是哪个插件拦截到的信息，像skywalking的服务端一样，使用统一的展示方式在前端展示数据。</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月17日 10:05:41
   * @Param [span, linkedList]
   **/
  private void getData2(SegmentDo segmentDo, Span span, List<String> linkedList) {
    try {
      ObjectNode jsonObject = JsonUtil.createJsonObject();
      int spanId = span.getSpanId();
      if (0 == spanId) {
        getSpringMvcInfo(span, jsonObject, linkedList);
      } else if (0 < span.getTags().size()) {
        jsonObject.put("spanId", spanId);
        jsonObject.put("parentSpanId", span.getParentSpanId());
        jsonObject.put("serviceCode", span.getServiceCode());
        jsonObject.put("serviceInstanceName", span.getServiceInstanceName());
        jsonObject.put(Const.START_TIME, span.getStartTime());
        jsonObject.put(Const.END_TIME, span.getEndTime());
        jsonObject.put("endpointName", span.getEndpointName());
        jsonObject.put("peer", span.getPeer());
        jsonObject.put("component", span.getComponent());
        List<LogEntity> logs = span.getLogs();
        if (null != logs && 0 < logs.size()) {
          jsonObject.put("logs", JsonUtil.obj2String(logs));
        }
        List<KeyValue> tags = span.getTags();
        if (0 < tags.size()) {
          Boolean flag = false;
          Boolean isSql = false;
          String dbUserName = null;
          String msSql = null;
          String msSchemaName = null;
          String key = null;
          String url = null;
          String httpBody = null;
          for (KeyValue tag : tags) {
            key = tag.getKey();
            if (segmentDo.getOperationName().equals("Jedis/sentinelGetMasterAddrByName")) {
              flag = true;
              break;
            }
            if (tag.getValue().equals("Redis")) {
              // 不再存储单纯的Redis请求；2022-06-30 16:34:24
              flag = true;
              break;
            }
            if (key.equals("http.body")) {
              // 不再存储单纯的GET请求；2022-05-27 18:14:25
              httpBody = tag.getValue();
            } else if (key.equals("url")) {
              // 不再存储单纯的GET请求；2022-05-27 18:14:25
              url = tag.getValue();
              if (!url.contains("dingtalk")) {
                flag = true;
                break;
              }
            } else if (key.equals("http.method")) {
              // 不再存储单纯的GET请求；2022-05-27 18:14:25
              if (StringUtil.isNotBlank(url) && !url.contains("dingtalk")) {
                flag = true;
                break;
              }
            } else if (key.equals("db.instance")) {
              msSchemaName = tag.getValue();
            } else if (key.equals("db_user_name")) {
              dbUserName = tag.getValue();
            } else if (key.equals("db.statement") && !key.equals("Redis")) {
              // 一开始的想法：这里需要对SQL语句进行规范化，否则无法将探针获取到的SQL与阿里云的SQL洞察获取到的SQL进行精确匹配；2022-05-27 21:12:13
              // 想法更改：这里不需要对SQL语句进行格式化了，因为skywalking的Java探针截取到的SQL语句有一定的格式，一般人很难在Navicat这样的工具中，来模仿Java探针的SQL语句格式。通过这个格式就可以简单区分来自SQL洞察中的skywalking探针发出的SQL；2022-05-28 12:48:12
              msSql = tag.getValue();
              isSql = true;
            }
          }

          getUserNameFromHttpBody(segmentDo, url, httpBody);

          // 已经不再往ms_audit_log表里插入数据了，所以这里注释掉；2022-07-01 09:27:49
          // if (true == isSQL && StringUtil.isNotBlank(msSql)) {
          //   // 将SQL组装成对象，并放入到list集合中；2022-05-28 13:22:45
          //   getMsAuditLogDo(segmentDo, msSql, span, msSchemaName, dbUserName, auditLogFromSkywalkingAgent);
          // }

          if (false == flag) {
            jsonObject.put("tags", JsonUtil.obj2String(tags));
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
            String username = String.valueOf(((LinkedHashMap) data).get("username"));
            if (StringUtil.isNotBlank(username) && StringUtil.isBlank(segmentDo.getUserName())) {
              segmentDo.setUserName(username);
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
      if (keyValue.getKey().equals("url")) {
        String url = keyValue.getValue();
        jsonObject.put("url", url);
        linkedList.add(jsonObject.toString());
      }
    }
  }

  /**
   * <B>方法名称：insertSegment2</B>
   * <B>概要说明：将segment数据插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月23日 10:05:54
   * @Param [segment]
   **/
  private void insertSegmentBySingle(SegmentDo segment) {
    int insertResult = segmentDao.insertSelective(segment);
    if (1 != insertResult) {
      log.error("开始执行 AiitKafkaConsumer # insertSegmentBySingle()方法，将完整的调用链 = 【{}】 插入到表中失败。", JsonUtil.obj2String(segment));
    }
  }

  /**
   * <B>方法名称：setUserNameTokenGlobalTraceIdToLocalMemory </B>
   * <B>概要说明：将userName或者token，与globalTraceId关联起来 </B>
   * 注：userName与token是等价的。当用户第一次登录时，如果用户校验成功，那么下次用户再访问其他接口时，会使用token来代替用户名。
   *
   * @return void
   * @Author zm
   * @Date 2022年05月23日 10:05:22
   * @Param [segment]
   **/
  private void setUserNameTokenGlobalTraceIdToLocalMemory(SegmentDo segment) {
    // Map<String/* globalTraceId */, String/* userName */> globalTraceIdAndUserNameMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndUserNameMap();
    // Map<String/* globalTraceId */, String/* token */> globalTraceIdAndTokenMap = SingletonLocalStatisticsMap.getGlobalTraceIdAndTokenMapMap();
    // Map<String/* token */, String/* userName */> tokenAndUserNameMap = SingletonLocalStatisticsMap.getTokenAndUserNameMap();

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

  /**
   * <B>方法名称：insertSegment</B>
   * <B>概要说明：将原始的用户访问行为信息插入到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月23日 14:08:17
   * @Param [segment, segmentObject, parentSegmentId]
   **/
  private void insertSegment(SegmentDo segment, SegmentObject segmentObject, String parentSegmentId) {
    // 用户名和token都是空的调用链，不存入数据库中。这里只存入带有用户名或者token完整的调用链。2022-04-20 16:35:52
    if (StringUtil.isBlank(segment.getUserName()) && StringUtil.isBlank(segment.getToken())) {
      // log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，该调用链 = 【{}】 不含有用户名和token，不能插入到表中。", JsonUtil.obj2String(segment));
      return;
    }

    int insertResult = segmentDao.insertSelective(segment);
    if (1 != insertResult) {
      log.error("开始执行 AiitKafkaConsumer # insertSegment()方法，将完整的调用链 = 【{}】 插入到表中失败。", JsonUtil.obj2String(segment));
    }
  }

  private List<Span> buildSpanList(SegmentObject segmentObject, SegmentDo segmentDo) {
    List<Span> spans = new ArrayList<>();

    List<SpanObject> spansList = segmentObject.getSpansList();
    if (null != spansList && 0 < spansList.size()) {
      segmentDo.setSpans(spansList.toString());
    }
    for (SpanObject spanObject : spansList) {
      String operationName = spanObject.getOperationName();

      // 忽略掉不需要的链路信息；2022-06-28 14:18:20
      Boolean flag = ignoreMethod(operationName);
      if (true == flag) {
        continue;
      }

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

    return spans;
  }

  /**
   * <B>方法名称：setUserNameAndTokenFromSpan</B>
   * <B>概要说明：从span中获取用户名和token</B>
   *
   * @return com.mingshi.skyflying.common.domain.SegmentDo
   * @Author zm
   * @Date 2022年07月12日 10:07:33
   * @Param [spanList, segment]
   **/
  private SegmentDo setUserNameAndTokenFromSpan(List<Span> spanList, SegmentDo segment) {
    try {
      if (null == segment) {
        segment = new SegmentDo();
      }

      // 为了制造千万级的数据，这里暂时不存储span的信息；2022-05-19 08:43:32
      // segment.setSpans(JsonUtil.obj2String(spanList));
      if (null == spanList && 0 == spanList.size()) {
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
   * <B>方法名称：setUserNameAndTokenFromSegmentObject</B>
   * <B>概要说明：从SegmentObject中获取用户名和token</B>
   *
   * @return com.mingshi.skyflying.common.domain.SegmentDo
   * @Author zm
   * @Date 2022年07月12日 10:07:59
   * @Param [spanList, segment]
   **/
  private SegmentDo setUserNameAndTokenFromSegmentObject(HashSet<String> userHashSet, SegmentDo segment, SegmentObject segmentObject) {
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
        if (false == userIsExisted) {
          userHashSet.add(userName);
        }
      }
      String token = segmentObject.getToken();
      if (StringUtil.isNotBlank(token)) {
        segment.setToken(token);
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.setUserNameAndTokenFromSegmentObject() # 从SegmentObject实例中获取用户名和token时，出现了异常。", e);
    }
    return segment;
  }
}

