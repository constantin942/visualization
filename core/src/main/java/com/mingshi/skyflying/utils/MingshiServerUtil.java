package com.mingshi.skyflying.utils;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.agent.AgentInformationSingleton;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.enums.ConstantsCode;
import com.mingshi.skyflying.common.utils.*;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.disruptor.processor.ProcessorByDisruptor;
import com.mingshi.skyflying.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.kafka.consumer.AiitKafkaConsumerRunner;
import com.mingshi.skyflying.kafka.consumer.MsKafkaSegmentsConsumer;
import com.mingshi.skyflying.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.queue.IoThreadBatchInsertByLinkedBlockingQueue;
import com.mingshi.skyflying.reactor.thread.ProcessorHandlerByLinkedBlockingQueue;
import com.mingshi.skyflying.sql.SqlTypeMap;
import com.mingshi.skyflying.statistics.InformationOverviewSingleton;
import lombok.extern.slf4j.Slf4j;
import net.sf.jsqlparser.JSQLParserException;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.OffsetAndMetadata;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.utils.Bytes;
import org.apache.kafka.common.utils.CopyOnWriteMap;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>主类名称: mingshiServerUtil</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * Date 2022/5/30 20:46
 * @Version 1.0
 **/
@Slf4j
@Component
public class MingshiServerUtil {
  @Value("${reactor.processor.enable}")
  private boolean reactorProcessorEnable;
  @Value("${reactor.processor.disruptor}")
  private boolean reactorProcessorDisruptor;
  @Value("${reactor.iothread.thread.count}")
  private Integer reactorIoThreadThreadCount;
  @Value("${reactor.processor.graceful-shutdown}")
  private boolean gracefulShutdown;
  @Value("${reactor.processor.graceful-shutdown-queue-size}")
  private Integer gracefulShutdownQueueSize;

  @Resource
  private AiitKafkaProducer aiitKafkaProducer;
  @Resource
  private ProcessorByDisruptor processorByDisruptor;
  @Resource
  private RedisPoolUtil redisPoolUtil;
  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private MsSegmentDetailUsernameIsNullMapper msSegmentDetailUsernameIsNullMapper;
  @Resource
  private MsAlarmInformationMapper msAlarmInformationMapper;
  @Resource
  private MsAgentInformationMapper msAgentInformationMapper;
  @Resource
  private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
  @Resource
  private SegmentDao segmentDao;
  @Resource
  private MingshiServerUtil mingshiServerUtil;
  @Resource
  private AiitKafkaConsumerRunner aiitKafkaConsumerRunner;

  /**
   * 产生字符串类型的订单号
   */
  public String getOrderId(String orderId) {
    if (StringUtil.isBlank(orderId)) {
      orderId = SnowflakeIdWorker.generateStringId();
    }
    return orderId;
  }

  /**
   * <B>方法名称：setDbTypeAndOperationType</B>
   * <B>概要说明：给MsSegmentDetailDo实例设置dbType类型和operationType类型</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月19日 09:08:00
   * @Param [msSegmentDetailDo, dbType, operationType, value]
   **/
  public void setDbTypeAndOperationType(MsSegmentDetailDo msSegmentDetailDo, String dbType, String operationType, String value) {
    msSegmentDetailDo.setDbType(dbType);
    msSegmentDetailDo.setDbStatement(value);
    msSegmentDetailDo.setOperationType(operationType);
  }

  /**
   * <B>方法名称：doEnableReactorModel</B>
   * <B>概要说明：将数据组装一下，然后放入到公共队列中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 15:08:05
   * @Param [map, spanList, esSegmentDetaiDolList, segmentDo, segmentDetaiDolList, segmentDetaiUserNameIsNullDolList, msAlarmInformationDoList, skywalkingAgentHeartBeatMap]
   **/
  public void doEnableReactorModel(ConsumerRecord<String, Bytes> consumerRecord,
                                   HashMap<String, Map<String, Integer>> map,
                                   List<Span> spanList,
                                   SegmentDo segmentDo,
                                   List<MsSegmentDetailDo> segmentDetaiDolList,
                                   List<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList,
                                   List<MsAlarmInformationDo> msAlarmInformationDoList,
                                   Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap) {
    try {
      ObjectNode jsonObject = JsonUtil.createJsonObject();
      // if (null != segmentDo) {
      //   jsonObject.put(Const.SEGMENT, JsonUtil.object2String(segmentDo));
      // }

      /**
       * 统计当前线程的QPS；2022-07-23 11:05:16
       */
      if (null != map && 0 < map.size()) {
        jsonObject.put(Const.QPS_ZSET_EVERY_PROCESSOR_THREAD, JsonUtil.obj2String(map));
      }
      if (null != segmentDetaiDolList && 0 < segmentDetaiDolList.size()) {
        jsonObject.put(Const.SEGMENT_DETAIL_DO_LIST, JsonUtil.obj2String(segmentDetaiDolList));
      }
      if (null != segmentDetaiUserNameIsNullDolList && 0 < segmentDetaiUserNameIsNullDolList.size()) {
        jsonObject.put(Const.SEGMENT_DETAIL_USERNAME_IS_NULL_DO_LIST, JsonUtil.obj2String(segmentDetaiUserNameIsNullDolList));
      }
      if (null != msAlarmInformationDoList && 0 < msAlarmInformationDoList.size()) {
        jsonObject.put(Const.ABNORMAL, JsonUtil.obj2String(msAlarmInformationDoList));
      }
      if (null != skywalkingAgentHeartBeatMap && 0 < skywalkingAgentHeartBeatMap.size()) {
        jsonObject.put(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, JsonUtil.obj2String(skywalkingAgentHeartBeatMap));
      }

      ObjectNode topicPartitionOffsetJson = getTopicPartitionOffset(consumerRecord);
      if (null != topicPartitionOffsetJson) {
        jsonObject.put(Const.TOPIC_PARTITION_OFFSET, topicPartitionOffsetJson.toString());
      }
      Integer partition = consumerRecord.partition();
      if (null != jsonObject && 0 < jsonObject.size()) {
        Integer queueIndex = IoThreadBatchInsertByLinkedBlockingQueue.getQueueIndex(partition);
        LinkedBlockingQueue linkedBlockingQueue = IoThreadBatchInsertByLinkedBlockingQueue.getLinkedBlockingQueue(gracefulShutdown, reactorIoThreadThreadCount, 10, mingshiServerUtil, partition);
        if (null != linkedBlockingQueue) {
          if (linkedBlockingQueue.size() == IoThreadBatchInsertByLinkedBlockingQueue.getQueueAllSize()) {
            log.error("将调用链信息放入到BatchInsertByLinkedBlockingQueue队列中，队列满了，当前队列中的元素个数【{}】，队列的容量【{}】。", linkedBlockingQueue.size(), IoThreadBatchInsertByLinkedBlockingQueue.getQueueAllSize());
            String key = DateTimeUtil.dateToStr(new Date());
            if (0 < linkedBlockingQueue.size()) {
              redisPoolUtil.zSetIncrementScore(Const.SECOND_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE + "-" + (1 + queueIndex), key, Double.valueOf(linkedBlockingQueue.size()));
            }
          }
          // 当第二层队列满了后，这里会阻塞住；2022-09-14 19:14:41
          linkedBlockingQueue.put(jsonObject);
        }
      }
    } catch (Exception e) {
      log.error("将清洗好的调用链信息放入到队列中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：getTopicPartitionOffset</B>
   * <B>概要说明：将topic、partition、offset放到一个json对象里</B>
   *
   * @return com.fasterxml.jackson.databind.node.ObjectNode
   * @Author zm
   * @Date 2022年09月14日 16:09:54
   * @Param [consumerRecord]
   **/
  private ObjectNode getTopicPartitionOffset(ConsumerRecord<String, Bytes> consumerRecord) {
    if (null == consumerRecord) {
      return null;
    }
    ObjectNode topicPartitionOffsetJson = JsonUtil.createJsonObject();
    topicPartitionOffsetJson.put(Const.TOPIC, consumerRecord.topic());
    topicPartitionOffsetJson.put(Const.PARTITION, consumerRecord.partition());
    topicPartitionOffsetJson.put(Const.OFFSET, consumerRecord.offset());
    return topicPartitionOffsetJson;
  }

  /**
   * <B>方法名称：synchronizationUserPortraitByVisitedTimeToLocalMemory</B>
   * <B>概要说明：同步用户访问过的表到本地内存</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月25日 17:07:11
   * @Param [userPortraitByVisitedTimeDo]
   **/
  public void synchronizationUserPortraitByVisitedTableToLocalMemory(UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo) {
    String userName = userPortraitByVisitedTableEverydayDo.getUserName();
    String visitedDate = userPortraitByVisitedTableEverydayDo.getVisitedDate();
    String tables = userPortraitByVisitedTableEverydayDo.getVisitedTable();
    String dbType = userPortraitByVisitedTableEverydayDo.getDbType();
    Integer visitedCount = userPortraitByVisitedTableEverydayDo.getVisitedCount();
    Map<String/* 用户名 */, Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */Integer/* 访问次数 */>>>> userPortraitByVisitedTableMap =
      AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableMap();
    if (null != userPortraitByVisitedTableMap) {
      Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */Integer/* 访问次数 */>>> stringMapMap = userPortraitByVisitedTableMap.get(userName);
      if (null == stringMapMap) {
        stringMapMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
        userPortraitByVisitedTableMap.put(userName, stringMapMap);
      }
      Map<String/* 访问日期，以天为单位 */, Map<String,/* 数据库操作类型：insert、delete、update、select */Integer/* 访问次数 */>> tablesMap = stringMapMap.get(tables);
      if (null == tablesMap) {
        tablesMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
        stringMapMap.put(tables, tablesMap);
      }
      Map<String, Integer> originalTimeMap = tablesMap.get(visitedDate);
      if (null == originalTimeMap) {
        originalTimeMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
        tablesMap.put(visitedDate, originalTimeMap);
      }
      originalTimeMap.put(dbType, visitedCount);
    }
  }

  /**
   * <B>方法名称：synchronizationUserPortraitByVisitedTimeToLocalMemory</B>
   * <B>概要说明：同步用户访问过的时间到本地内存</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月25日 17:07:11
   * @Param [userPortraitByVisitedTimeDo]
   **/
  public void synchronizationUserPortraitByVisitedTimeToLocalMemory(UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo) {
    String userName = userPortraitByVisitedTimeDo.getUserName();
    Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> userPortraitByVisitedTimeMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
    Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */> map = userPortraitByVisitedTimeMap.get(userName);
    Integer forenoonCount = userPortraitByVisitedTimeDo.getForenoonCount();
    Integer afternoonCount = userPortraitByVisitedTimeDo.getAfternoonCount();
    Integer nightCount = userPortraitByVisitedTimeDo.getNightCount();
    if (null == map) {
      map = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
      userPortraitByVisitedTimeMap.put(userName, map);
    }
    if (null != forenoonCount) {
      map.put(ConstantsCode.USER_PORTRAIT_FORENOON.getCode(), forenoonCount);
    }
    if (null != afternoonCount) {
      map.put(ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode(), afternoonCount);
    }
    if (null != nightCount) {
      map.put(ConstantsCode.USER_PORTRAIT_NIGHT.getCode(), nightCount);
    }
  }

  /**
   * <B>方法名称：getSqlType</B>
   * <B>概要说明：获取SQL语句的类型</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年05月28日 12:05:33
   * @Param [msSql]
   **/
  public String getSqlType(String msSql) {
    String sqlTypeFromLibrary = null;
    try {
      String sqlType = SqlParserUtils.getSqlType(msSql);
      if(StringUtil.isNotBlank(sqlType)){
        sqlTypeFromLibrary = sqlType.toLowerCase();
      }
    } catch (JSQLParserException e) {
      log.error("# MingshiServerUtil.getSqlType() # 根据SQL语句 = 【{}】获取sql类型时，出现了异常。", e);
    }

    String sqlType = null;
    if (StringUtil.isBlank(sqlTypeFromLibrary)) {
      sqlType = doGetSqlType(msSql);
    }

    log.error("#SegmentConsumeServiceImpl.getSqlType() #没有匹配到SQL的类型，这是不正常的。需要好好的排查下，当前SQL = 【{}】。", msSql);
    if (StringUtil.isNotBlank(sqlType) && !sqlType.equals(sqlTypeFromLibrary)) {
      log.error("#SegmentConsumeServiceImpl.getSqlType() # 根据SQL语句 = 【{}】从库里获取到的sql类型 = 【{}】与原生匹配到的sql类型 = 【{}】不一致。", msSql, sqlTypeFromLibrary, sqlType);
    } else {
      sqlType = sqlTypeFromLibrary;
    }
    return sqlType;
  }

  /**
   * <B>方法名称：doGetSqlType</B>
   * <B>概要说明：获取sql类型</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年09月20日 09:09:23
   * @Param [msSql]
   **/
  private String doGetSqlType(String msSql) {
    String sqlType = null;
    if (msSql.startsWith(Const.SQL_TYPE_REVOKE) || msSql.startsWith(Const.SQL_TYPE_REVOKE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_REVOKE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_REVOKE)) {
      sqlType = Const.SQL_TYPE_REVOKE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_GRANT) || msSql.startsWith(Const.SQL_TYPE_GRANT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_GRANT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_GRANT)) {
      sqlType = Const.SQL_TYPE_GRANT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_SELECT) || msSql.startsWith(Const.SQL_TYPE_SELECT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SELECT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SELECT)) {
      sqlType = Const.SQL_TYPE_SELECT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_INSERT) || msSql.startsWith(Const.SQL_TYPE_INSERT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_INSERT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_INSERT)) {
      sqlType = Const.SQL_TYPE_INSERT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_UPDATE) || msSql.startsWith(Const.SQL_TYPE_UPDATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_UPDATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_UPDATE)) {
      sqlType = Const.SQL_TYPE_UPDATE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DELETE) || msSql.startsWith(Const.SQL_TYPE_DELETE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DELETE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DELETE)) {
      sqlType = Const.SQL_TYPE_DELETE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_LOGIN) || msSql.startsWith(Const.SQL_TYPE_LOGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGIN)) {
      sqlType = Const.SQL_TYPE_LOGIN.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_LOGOUT) || msSql.startsWith(Const.SQL_TYPE_LOGOUT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGOUT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_LOGOUT)) {
      sqlType = Const.SQL_TYPE_LOGOUT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_MERGE) || msSql.startsWith(Const.SQL_TYPE_MERGE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_MERGE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_MERGE)) {
      sqlType = Const.SQL_TYPE_MERGE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_ALTER) || msSql.startsWith(Const.SQL_TYPE_ALTER.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ALTER.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ALTER)) {
      sqlType = Const.SQL_TYPE_ALTER.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_CREATEINDEX) || msSql.startsWith(Const.SQL_TYPE_CREATEINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATEINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATEINDEX)) {
      sqlType = Const.SQL_TYPE_CREATEINDEX.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DROPINDEX) || msSql.startsWith(Const.SQL_TYPE_DROPINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROPINDEX.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROPINDEX)) {
      sqlType = Const.SQL_TYPE_DROPINDEX.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_CREATE) || msSql.startsWith(Const.SQL_TYPE_CREATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CREATE)) {
      sqlType = Const.SQL_TYPE_CREATE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DROP) || msSql.startsWith(Const.SQL_TYPE_DROP.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROP.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DROP)) {
      sqlType = Const.SQL_TYPE_DROP.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_SET) || msSql.startsWith(Const.SQL_TYPE_SET.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SET.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SET)) {
      sqlType = Const.SQL_TYPE_SET.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DESC) || msSql.startsWith(Const.SQL_TYPE_DESC.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESC.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESC)) {
      sqlType = Const.SQL_TYPE_DESC.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_REPLACE) || msSql.startsWith(Const.SQL_TYPE_REPLACE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_REPLACE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_REPLACE)) {
      sqlType = Const.SQL_TYPE_REPLACE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_CALL) || msSql.startsWith(Const.SQL_TYPE_CALL.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CALL.toLowerCase()) || msSql.contains(Const.SQL_TYPE_CALL)) {
      sqlType = Const.SQL_TYPE_CALL.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_BEGIN) || msSql.startsWith(Const.SQL_TYPE_BEGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_BEGIN.toLowerCase()) || msSql.contains(Const.SQL_TYPE_BEGIN)) {
      sqlType = Const.SQL_TYPE_BEGIN.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_DESCRIBE) || msSql.startsWith(Const.SQL_TYPE_DESCRIBE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESCRIBE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_DESCRIBE)) {
      sqlType = Const.SQL_TYPE_DESCRIBE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_ROLLBACK) || msSql.startsWith(Const.SQL_TYPE_ROLLBACK.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ROLLBACK.toLowerCase()) || msSql.contains(Const.SQL_TYPE_ROLLBACK)) {
      sqlType = Const.SQL_TYPE_ROLLBACK.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_FLUSH) || msSql.startsWith(Const.SQL_TYPE_FLUSH.toLowerCase()) || msSql.contains(Const.SQL_TYPE_FLUSH.toLowerCase()) || msSql.contains(Const.SQL_TYPE_FLUSH)) {
      sqlType = Const.SQL_TYPE_FLUSH.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_USE) || msSql.startsWith(Const.SQL_TYPE_USE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_USE.toLowerCase()) || msSql.contains(Const.SQL_TYPE_USE)) {
      sqlType = Const.SQL_TYPE_USE.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_SHOW) || msSql.startsWith(Const.SQL_TYPE_SHOW.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SHOW.toLowerCase()) || msSql.contains(Const.SQL_TYPE_SHOW)) {
      sqlType = Const.SQL_TYPE_SHOW.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_START) || msSql.startsWith(Const.SQL_TYPE_START.toLowerCase()) || msSql.contains(Const.SQL_TYPE_START.toLowerCase()) || msSql.contains(Const.SQL_TYPE_START)) {
      sqlType = Const.SQL_TYPE_START.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_COMMIT) || msSql.startsWith(Const.SQL_TYPE_COMMIT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_COMMIT.toLowerCase()) || msSql.contains(Const.SQL_TYPE_COMMIT)) {
      sqlType = Const.SQL_TYPE_COMMIT.toLowerCase();
    } else if (msSql.startsWith(Const.SQL_TYPE_RENAME) || msSql.startsWith(Const.SQL_TYPE_RENAME.toLowerCase()) || msSql.contains(Const.SQL_TYPE_RENAME.toLowerCase()) || msSql.contains(Const.SQL_TYPE_RENAME)) {
      sqlType = Const.SQL_TYPE_RENAME.toLowerCase();
    } else if (Const.KEYS_ALL.equals(msSql)) {
      sqlType = null;
    } else {
      sqlType = null;
    }
    return sqlType;
  }

  /**
   * <B>方法名称：getTableName</B>
   * <B>概要说明：根据SQL类型和SQL语句，获取表名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年06月06日 14:06:39
   * @Param [sqlType, msSql]
   **/
  public String getTableName(String sqlType, String msSql) {
    String tableName = null;
    if (StringUtil.isBlank(sqlType)) {
      return tableName;
    }
    List<String> tableNameList = null;
    if (sqlType.equals(Const.SQL_TYPE_SELECT.toLowerCase())) {
      tableNameList = SqlParserUtils.selectTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_INSERT.toLowerCase())) {
      tableNameList = SqlParserUtils.insertTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_UPDATE.toLowerCase())) {
      tableNameList = SqlParserUtils.updateTable(msSql);
    } else if (sqlType.equals(Const.SQL_TYPE_DELETE.toLowerCase())) {
      tableNameList = SqlParserUtils.deleteTable(msSql);
    } else {
      log.error("# SegmentConsumeServiceImpl.getMsAuditLogDo() # 根据SQL语句 = 【{}】获取表名时，该SQL语句不是select、insert、update、delete。", msSql);
    }
    if (null != tableNameList && 0 < tableNameList.size()) {
      for (String table : tableNameList) {
        if (StringUtil.isBlank(tableName)) {
          tableName = table;
        } else {
          tableName = tableName + Const.COMMA + table;
        }
      }
    }
    return tableName;
  }

  public List<String> getTableNameList(String sqlType, String msSql) {
    List<String> tableNameList = null;
    if (StringUtil.isBlank(sqlType)) {
      return tableNameList;
    }

    return SqlTypeMap.getSqlTable(sqlType, msSql);
  }

  /**
   * <B>方法名称：batchUpdateMsAuditLog</B>
   * <B>概要说明：批量更新审计日志的用户名和globalTraceId信息；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月01日 11:06:48
   * @Param [auditLogDoList]
   **/
  private void batchUpdateMsSegmentDetail(List<MsSegmentDetailDo> setmentDetailDoList) {
    try {
      if (0 < setmentDetailDoList.size()) {
        Instant now = Instant.now();
        msSegmentDetailDao.updateBatch(setmentDetailDoList);
        log.info("# IoThread.batchUpdateMsSegmentDetail # 更新数据库审计数据（【{}】条）的用户名耗时【{}】毫秒。", setmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# IoThread.batchUpdateMsSegmentDetail # 批量更新审计日志中的登录应用系统的用户名时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：flushToDB</B>
   * <B>概要说明：批量插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月19日 18:05:20
   * @Param []
   **/
  public void flushSegmentToDb(LinkedList<SegmentDo> segmentList) {
    if (0 < segmentList.size()) {
      try {
        Instant now = Instant.now();
        segmentDao.insertSelectiveBatch(segmentList);
        log.info("将【{}】条segment数据插入到表中，耗时【{}】毫秒。", segmentList.size(), DateTimeUtil.getTimeMillis(now));
        segmentList.clear();
      } catch (Exception e) {
        log.error("将segment数据批量插入到数据库中的时候，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSegmentDetailCountToRedis</B>
   * <B>概要说明：实时segmentDetail数据的统计数量保存到Redis的哈希表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月18日 16:07:28
   * @Param [count]
   **/
  public void flushSegmentDetailCountToRedis(LinkedList<MsSegmentDetailDo> list) {
    Instant now = Instant.now();
    if (null != list && 0 < list.size()) {
      Integer count = list.size();
      try {
        Map<String, Integer> map = new HashMap<>(Const.NUMBER_EIGHT);
        for (MsSegmentDetailDo msSegmentDetailDo : list) {
          String userName = msSegmentDetailDo.getUserName();
          String startTime = msSegmentDetailDo.getStartTime();
          String tableName = msSegmentDetailDo.getMsTableName();
          String dbInstance = msSegmentDetailDo.getDbInstance();
          String dbType = msSegmentDetailDo.getDbType();
          String peer = msSegmentDetailDo.getPeer();
          String serviceCode = msSegmentDetailDo.getServiceCode();
          if (StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(peer) && StringUtil.isNotBlank(dbInstance) && StringUtil.isNotBlank(tableName)) {
            // 信息概况 -> 用户访问行为
            flushUserAccessBehaviorToRedis(dbType, userName, startTime, peer, dbInstance, tableName, serviceCode);

            // 根据年月日，统计每天的访问次数；2022-07-20 14:11:55
            statisticVisitedCountByEveryday(msSegmentDetailDo, map);
          }
        }

        // 更新每天采集情况和总的采集情况到Redis；2022-07-20 14:17:03
        updateEverydayStatisticToRedis(map);

        log.info("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时统计【{}】条segmentDetail数据到Redis的哈希表中，耗时【{}】毫秒。", count, DateTimeUtil.getTimeMillis(now));
      } catch (Exception e) {
        log.error("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时segmentDetail数据的统计数量保存到Redis的哈希表中，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：updateEverydayStatisticToRedis</B>
   * <B>概要说明：更新每天采集情况和总的采集情况到Redis</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 14:07:45
   * @Param [map]
   **/
  private void updateEverydayStatisticToRedis(Map<String, Integer> map) {
    try {
      Iterator<String> iterator = map.keySet().iterator();
      while (iterator.hasNext()) {
        String key = iterator.next();
        Integer value = map.get(key);
        // 更新每天采集情况；
        redisPoolUtil.hsetIncrBy(Const.HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS, key, value.longValue());
        // 更新总的采集情况；
        redisPoolUtil.incr(Const.STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS, value.longValue());
      }
    } catch (Exception e) {
      log.error("# MingshiServerUtil.updateEverydayStatisticToRedis() # 更新每天采集情况和总的采集情况到Redis时，出现了异常。 ", e);
    }
  }

  /**
   * <B>方法名称：statisticVisitedCountByEveryday</B>
   * <B>概要说明：根据年月日，统计每天的访问次数；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 14:07:15
   * @Param [msSegmentDetailDo, map]
   **/
  private void statisticVisitedCountByEveryday(MsSegmentDetailDo msSegmentDetailDo, Map<String, Integer> map) {
    try {
      String startTimeOld = msSegmentDetailDo.getStartTime();
      Date date = DateTimeUtil.strToDate(startTimeOld);
      String startTimeNew = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);

      Integer value = map.get(startTimeNew);
      if (null == value) {
        map.put(startTimeNew, 1);
      } else {
        map.put(startTimeNew, value + 1);
      }
    } catch (Exception e) {
      log.error("# MingshiServerUtil.statisticVisitedCountByEveryday() #根据年月日，统计每天的访问次数时，出现了异常。 ", e);
    }
  }

  /**
   * <B>方法名称：flushUserNameToRedis</B>
   * <B>概要说明：将用户名发送到redis中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月19日 10:07:28
   * @Param [userHashSet]
   **/
  public void flushUserNameToRedis(HashSet<String> userHashSet) {
    if (null != userHashSet && 0 < userHashSet.size()) {
      Integer count = userHashSet.size();
      try {
        Instant now = Instant.now();
        for (String userName : userHashSet) {
          redisPoolUtil.sadd(Const.SET_DATA_STATISTICS_HOW_MANY_USERS, userName);
          // 将用户名放到本地内存中；2022-07-19 10:12:13
          InformationOverviewSingleton.put(userName);
        }
        log.info("# MingshiServerUtil.flushUserNameToRedis() # 实时统计将【{}】条用户名发送到redis中，耗时【{}】毫秒。", count, DateTimeUtil.getTimeMillis(now));
        userHashSet.clear();
      } catch (Exception e) {
        log.error("# MingshiServerUtil.flushUserNameToRedis() # 实时将用户名发送到redis中，出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushUserAccessBehaviorToRedis</B>
   * <B>概要说明：实时将用户访问行为信息发送到redis中 </B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 14:07:54
   * @Param [userName, startTime, peer, dbInstance, tableName]
   **/
  private void flushUserAccessBehaviorToRedis(String dbType, String userName, String startTime, String peer, String dbInstance, String tableName, String serviceCode) {
    try {
      // 用户访问次数 + 1；
      redisPoolUtil.incr(Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + userName, 1);
      if (StringUtil.isNotBlank(startTime)) {
        // 更新用户对数据库最后的访问时间；
        redisPoolUtil.set(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + userName, startTime);
      }

      doFlushUserAccessBehaviorToRedis(dbType, peer, dbInstance, tableName, userName, startTime, serviceCode);

    } catch (Exception e) {
      log.error("# MingshiServerUtil.doFlushUserAccessBehaviorToRedis() # 实时将用户访问行为信息发送到redis中，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：doFlushUserAccessBehaviorToRedis</B>
   * <B>概要说明：实时将用户访问行为信息发送到redis中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月20日 17:07:45
   * @Param [peer, dbInstance, tableName, userName, startTime]
   **/
  private void doFlushUserAccessBehaviorToRedis(String dbType, String peer, String dbInstance, String tableName, String userName, String startTime, String serviceCode) {
    // 累加用户对数据库表资源的访问次数；
    String zsetVlue = doGetTableName(peer, dbInstance, tableName);
    String serviceCodeName = AgentInformationSingleton.get(serviceCode);
    serviceCode = serviceCodeName == Const.DOLLAR ? serviceCode : serviceCodeName;

    Date date = DateTimeUtil.strToDate(startTime);
    String startTimeNew = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);

    if (tableName.contains(Const.COMMA)) {
      String[] split = tableName.split(Const.COMMA);
      for (String tn : split) {
        // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
        zsetVlue = doGetTableName(peer, dbInstance, tn);
        // 对每一个表，统计每天的访问次数；2022-07-22 10:42:33
        redisPoolUtil.hsetIncrBy(Const.HASH_TABLE_EVERYDAY_VISITED_TIMES + zsetVlue, startTimeNew, 1L);
        // 将用户访问过的表放到这个用户对应的有序集合zset中；2022-07-20 14:30:07
        redisPoolUtil.zSetIncrementScore(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName, zsetVlue, 1);
        // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
        redisPoolUtil.zSetIncrementScore(Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue, serviceCode + Const.DOLLAR + userName, 1);

        // 有序集合：存放对每个表操作类型统计；2022-07-22 15:47:48
        redisPoolUtil.zSetIncrementScore(Const.ZSET_TABLE_OPERATION_TYPE + zsetVlue, dbType, 1);
        // 记录每一个数据库表最后的被访问的时间；
        redisPoolUtil.set(Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue, startTime);
        LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(zsetVlue);
      }
    } else {
      // 对每一个表，统计每天的访问次数；2022-07-22 10:42:33
      redisPoolUtil.hsetIncrBy(Const.HASH_TABLE_EVERYDAY_VISITED_TIMES + zsetVlue, startTimeNew, 1L);
      // 将用户访问过的表放到这个用户对应的有序集合zset中；2022-07-20 14:30:07
      redisPoolUtil.zSetIncrementScore(Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName, zsetVlue, 1);
      // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
      redisPoolUtil.zSetIncrementScore(Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue, serviceCode + Const.DOLLAR + userName, 1);

      // 有序集合：存放对每个表操作类型统计；2022-07-22 15:47:48
      redisPoolUtil.zSetIncrementScore(Const.ZSET_TABLE_OPERATION_TYPE + zsetVlue, dbType, 1);
      // 记录每一个数据库表最后的被访问的时间；
      redisPoolUtil.set(Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue, startTime);
      LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(zsetVlue);
    }
    // 有序集合：统计每个用户操作类型次数；
    redisPoolUtil.zSetIncrementScore(Const.ZSET_USER_OPERATION_TYPE + userName, dbType, 1);

  }

  /**
   * <B>方法名称：flushAbnormalToDB</B>
   * <B>概要说明：将异常信息批量插入到MySQL中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月07日 18:06:24
   * @Param [segmentDetaiDolList]
   **/
  public void flushAbnormalToDb(List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist) {
    if (0 < msAlarmInformationDoLinkedListist.size()) {
      try {
        Instant now = Instant.now();
        msAlarmInformationMapper.insertSelectiveBatch(msAlarmInformationDoLinkedListist);
        log.info("#SegmentConsumeServiceImpl.flushAbnormalToDB()# 将异常信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", msAlarmInformationDoLinkedListist.size(), DateTimeUtil.getTimeMillis(now));
        msAlarmInformationDoLinkedListist.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushAbnormalToDB() # 将异常信息批量插入到MySQL中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSkywalkingAgentNameToRedis</B>
   * <B>概要说明：将探针信息发送到MySQL中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月27日 13:06:22
   * @Param [segmentDetailDoList]
   **/
  public void flushSkywalkingAgentInformationToDb() {
    try {
      AtomicBoolean atomicBoolean = AgentInformationSingleton.getAtomicBoolean();
      if (atomicBoolean.get() == false) {
        // 只有当数据有变动时，才将其刷入到数据库中；2022-06-28 17:35:54
        return;
      }
      CopyOnWriteMap<String, String> instance = AgentInformationSingleton.getInstance();
      if (null != instance && 0 < instance.size()) {
        Instant now = Instant.now();
        LinkedList<MsAgentInformationDo> list = new LinkedList<>();
        Iterator<String> iterator = instance.keySet().iterator();
        while (iterator.hasNext()) {
          String key = iterator.next();
          if (StringUtil.isBlank(key)) {
            continue;
          }
          MsAgentInformationDo msAgentInformationDo = new MsAgentInformationDo();
          msAgentInformationDo.setAgentCode(key);
          list.add(msAgentInformationDo);
        }
        msAgentInformationMapper.insertBatch(list);
        // 本次刷新过后，只有当真的有数据变更后，下次才将其刷入到MySQL中；2022-06-28 17:51:11
        AgentInformationSingleton.setAtomicBooleanToFalse();
        log.info("#SegmentConsumeServiceImpl.flushSkywalkingAgentInformationToDb()# 将探针名称信息【{}条】批量插入到MySQL数据库中耗时【{}】毫秒。", instance.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.flushSkywalkingAgentInformationToDb() # 将探针名称信息批量插入到MySQL数据库中出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：flushSkywalkingAgentNameToRedis</B>
   * <B>概要说明：将探针信息发送到Redis中，用于计算探针心跳</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月27日 13:06:22
   * @Param [segmentDetailDoList]
   **/
  public void flushSkywalkingAgentNameToRedis(Map<String, String> skywalkingAgentTimeMap) {
    if (null != skywalkingAgentTimeMap && 0 < skywalkingAgentTimeMap.size()) {
      try {
        Set<String> stringSet = skywalkingAgentTimeMap.keySet();
        for (String set : stringSet) {
          Map<String, String> map = JsonUtil.string2Obj(set, Map.class);
          String serviceCode = map.get(Const.SERVICE_CODE);
          String value = AgentInformationSingleton.get(serviceCode);
          if (StringUtil.isBlank(value)) {
            AgentInformationSingleton.put(serviceCode, Const.DOLLAR);
            AgentInformationSingleton.setAtomicBooleanToTrue();
          }
        }

        Instant now = Instant.now();
        redisPoolUtil.hsetBatch(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, skywalkingAgentTimeMap);
        log.info("#SegmentConsumeServiceImpl.flushSkywalkingAgentNameToRedis()# 将探针名称信息【{}条】批量插入到Redis中耗时【{}】毫秒。", skywalkingAgentTimeMap.size(), DateTimeUtil.getTimeMillis(now));
        skywalkingAgentTimeMap.clear();
      } catch (Exception e) {
        log.error("# SegmentConsumeServiceImpl.flushSkywalkingAgentNameToRedis() # 将探针名称信息批量插入到Redis中出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：flushSegmentDetailToDb</B>
   * <B>概要说明：将用户操作信息保存到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月19日 16:08:43
   * @Param [segmentDetailDoList]
   **/
  public void flushSegmentDetailToDb(LinkedList<MsSegmentDetailDo> segmentDetailDoList) {
    if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {

      try {
        for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailDoList) {
          aiitKafkaProducer.send(msSegmentDetailDo, "flink-test-topic");
        }
      } catch (Exception e) {
        e.printStackTrace();
      }

      Instant now = Instant.now();
      try {
        msSegmentDetailDao.insertSelectiveBatch(segmentDetailDoList);
      } catch (Exception e) {
        log.error("#SegmentConsumeServiceImpl.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到MySQL中出现了异常。", segmentDetailDoList.size(), e);
      }
      // 实时segmentDetail数据的统计数量保存到Redis的哈希表中
      flushSegmentDetailCountToRedis(segmentDetailDoList);

      log.info("#SegmentConsumeServiceImpl.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", segmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
      segmentDetailDoList.clear();
    }
  }

  /**
   * <B>方法名称：flushSegmentDetailUserNameIsNullToDB</B>
   * <B>概要说明：将用户名为空的记录，保存到表中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 14:08:28
   * @Param [segmentDetaiUserNameIsNullDolList]
   **/
  public void flushSegmentDetailUserNameIsNullToDb(LinkedList<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList) {
    if (null != segmentDetaiUserNameIsNullDolList && 0 < segmentDetaiUserNameIsNullDolList.size()) {
      msSegmentDetailUsernameIsNullMapper.insertSelectiveBatch(segmentDetaiUserNameIsNullDolList);
      segmentDetaiUserNameIsNullDolList.clear();
    }
  }

  /**
   * <B>方法名称：getTableName</B>
   * <B>概要说明：获取表名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年07月15日 11:07:56
   * @Param [msMonitorBusinessSystemTablesDo]
   **/
  public String getTableName(MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo) {
    String key = "";
    if (null != msMonitorBusinessSystemTablesDo) {
      String dbAddress = msMonitorBusinessSystemTablesDo.getDbAddress();
      String dbName = msMonitorBusinessSystemTablesDo.getDbName();
      String tableName = msMonitorBusinessSystemTablesDo.getTableName();
      return doGetTableName(dbAddress, dbName, tableName);
    }
    return key;
  }

  public String doGetTableName(String dbAddress, String dbName, String tableName) {
    String key = "";
    if (StringUtil.isNotBlank(dbAddress)) {
      key = dbAddress + Const.POUND_KEY;
    }
    if (StringUtil.isNotBlank(dbName)) {
      key += dbName + Const.POUND_KEY;
    }
    if (StringUtil.isNotBlank(tableName)) {
      key += tableName;
    }
    return key;
  }

  /**
   * <B>方法名称：insertMonitorTables</B>
   * <B>概要说明：将业务系统中不存在的表批量插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月13日 14:07:03
   * @Param []
   **/
  public void insertMonitorTables() {
    try {
      Map<String, Integer> isChangedMap = LoadAllEnableMonitorTablesFromDb.getIsChangedMap();
      if (null != isChangedMap && 0 < isChangedMap.size()) {
        Set<String> keySet = isChangedMap.keySet();
        LinkedList<MsMonitorBusinessSystemTablesDo> list = new LinkedList<>();
        for (String tables : keySet) {
          String[] splits = tables.split(Const.POUND_KEY);
          if (Const.NUMBER_THREE == splits.length) {
            MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo = new MsMonitorBusinessSystemTablesDo();
            msMonitorBusinessSystemTablesDo.setDbAddress(splits[0]);
            msMonitorBusinessSystemTablesDo.setDbName(splits[1]);
            msMonitorBusinessSystemTablesDo.setTableName(splits[2]);
            list.add(msMonitorBusinessSystemTablesDo);
          }
        }
        if (0 < list.size()) {
          msMonitorBusinessSystemTablesMapper.insertSelectiveBatch(list);
        }
        isChangedMap.clear();
      }
    } catch (Exception e) {
      log.error("# SegmentConsumeServiceImpl.insertMonitorTables() # 将监管表中不存在的表插入到监管表中，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：flushProcessorThreadQpsToRedis</B>
   * <B>概要说明：将每一个processor线程的QPS发送到Redis中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月23日 11:07:52
   * @Param [processorThreadQpsMap]
   **/
  public void flushProcessorThreadQpsToRedis(Map<String, Map<String, Integer>> processorThreadQpsMap) {
    try {
      if (null == processorThreadQpsMap || 0 == processorThreadQpsMap.size()) {
        return;
      }

      Iterator<String> iterator = processorThreadQpsMap.keySet().iterator();
      while (iterator.hasNext()) {
        String threadName = iterator.next();
        Map<String, Integer> map = processorThreadQpsMap.get(threadName);
        if (null == map || 0 == map.size()) {
          continue;
        }
        Iterator<String> iterator1 = map.keySet().iterator();
        while (iterator1.hasNext()) {
          String time = iterator1.next();
          Integer count = map.get(time);
          // 统计所有Processor线程总的QPS；2022-07-27 10:16:30
          redisPoolUtil.zSetIncrementScore(Const.QPS_ZSET_ALL_PROCESSOR_THREAD, time, Long.valueOf(count));
        }
      }
      processorThreadQpsMap.clear();
    } catch (Exception e) {
      log.error("# MingshiServerUtil.flushProcessorThreadQpsToRedis() # 将每一个processor线程的QPS发送到Redis中的时候，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：statisticsProcessorAndIoThreadQueueSize</B>
   * <B>概要说明：将Processor线程和IoThread线程对应的队列大小发送到Redis中进行统计</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月25日 09:07:39
   * @Param []
   **/
  public void statisticsProcessorAndIoThreadQueueSize() {
    String name = Thread.currentThread().getName();
    String key = DateTimeUtil.dateToStrYyyyMmDdHhMmSs(new Date()) + "-" + name;
    if (true == reactorProcessorDisruptor) {
      long queueSize = processorByDisruptor.getQueueSize();
      if (0 < queueSize) {
        redisPoolUtil.zSetIncrementScore(Const.FIRST_QUEUE_SIZE_ZSET_BY_DISRUPTOR, key, Double.valueOf(queueSize));
      }
    } else {
      List<ProcessorHandlerByLinkedBlockingQueue> processorHandlerByLinkedBlockingQueueList = InitProcessorByLinkedBlockingQueue.getProcessorHandlerByLinkedBlockingQueueList();
      if (null != processorHandlerByLinkedBlockingQueueList && 0 < processorHandlerByLinkedBlockingQueueList.size()) {
        for (int i = 0; i < processorHandlerByLinkedBlockingQueueList.size(); i++) {
          Integer queueSize = processorHandlerByLinkedBlockingQueueList.get(i).getQueueSize();
          if (0 < queueSize) {
            redisPoolUtil.zSetIncrementScore(Const.FIRST_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE + "-" + (1 + i), key, Double.valueOf(queueSize));
          }
        }
      }
    }
    if (true == reactorProcessorEnable) {
      List<LinkedBlockingQueue<ObjectNode>> linkedBlockingQueueList = IoThreadBatchInsertByLinkedBlockingQueue.getLinkedBlockingQueueList();
      if (null != linkedBlockingQueueList && 0 < linkedBlockingQueueList.size()) {
        for (int i = 0; i < linkedBlockingQueueList.size(); i++) {
          Integer size = linkedBlockingQueueList.get(i).size();
          if (0 < size) {
            redisPoolUtil.zSetIncrementScore(Const.SECOND_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE + "-" + (1 + i), key, size.doubleValue());
          }
        }
      }
    }
  }

  /**
   * <B>方法名称：doInsertSegmentDetailIntoMySQLAndRedis</B>
   * <B>概要说明：将数据持久化到MySQL和Redis中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月27日 15:07:45
   * @Param [userHashSet, processorThreadQpsMap, segmentList, spanList, skywalkingAgentHeartBeatMap, segmentDetailDoList, msAlarmInformationDoLinkedListist]
   **/
  public void doInsertSegmentDetailIntoMySqlAndRedis(Map<String, Map<Integer, Long>> offsetsMap,
                                                     HashSet<String> userHashSet,
                                                     Map<String, Map<String, Integer>> processorThreadQpsMap,
                                                     LinkedList<SegmentDo> segmentList,
                                                     LinkedList<Span> spanList,
                                                     Map<String, String> skywalkingAgentHeartBeatMap,
                                                     LinkedList<MsSegmentDetailDo> segmentDetailDoList,
                                                     LinkedList<MsSegmentDetailDo> segmentDetailUserNameIsNullDoList,
                                                     List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist) {

    flushUserNameToRedis(userHashSet);

    // 将processor线程的QPS发送到Redis中；2022-07-23 11:22:13
    flushProcessorThreadQpsToRedis(processorThreadQpsMap);

    // 将公共队列中有多少元素没有被消费发送到Redis中统计；2022-07-23 11:33:39
    statisticsProcessorAndIoThreadQueueSize();

    // 统计kafka消费者每秒拿到多少消息；2022-07-28 13:57:05
    // statisticsKafkaConsumerRecords();

    // flushSegmentToDB(segmentList);

    // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
    flushSkywalkingAgentInformationToDb();

    // 将探针名称发送到Redis中，用于心跳检测；2022-06-27 13:42:13
    flushSkywalkingAgentNameToRedis(skywalkingAgentHeartBeatMap);

    insertMonitorTables();

    flushSegmentDetailToDb(segmentDetailDoList);

    flushSegmentDetailUserNameIsNullToDb(segmentDetailUserNameIsNullDoList);

    flushAbnormalToDb(msAlarmInformationDoLinkedListist);

    /**
     * 提交这批消息的offset到Kafka服务端
     */
    aiitCommitOffset(offsetsMap);
  }

  /**
   * <B>方法名称：aiitCommitOffset</B>
   * <B>概要说明：提交offset到Kafka服务端</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年09月13日 16:09:09
   * @Param [offsetsMap]
   **/
  private void aiitCommitOffset(Map<String, Map<Integer, Long>> offsetsMap) {
    if (null != offsetsMap && 0 < offsetsMap.size()) {
      try {
        if (true == gracefulShutdown) {
          MsKafkaSegmentsConsumer msKafkaSegmentsConsumer = aiitKafkaConsumerRunner.getMsKafkaSegmentsConsumer();
          Map<TopicPartition, OffsetAndMetadata> topicPartitionOffsetAndMetadataMap = buildCommits(offsetsMap);
          if (null != topicPartitionOffsetAndMetadataMap && 0 < topicPartitionOffsetAndMetadataMap.size()) {
            // 注意：一旦队列满了后，当前线程就会丢弃这个topic对应的partition和offset。就算这里丢弃这个信息也没关系，因为后续过来的消息比这个消息新就可以了。
            // 比如 topic = skywalking-segments，partition = 9，offset = 8763770。就算这条消息在队列满的情况下丢了也没关系，之后后续 这个topic = skywalking-segments，partition = 9中的offset > 8763770 就没关系。
            // 极端情况下，后续没有比这个topic = skywalking-segments，partition = 9中的offset 大的数，最坏的情况就是重复消费一下这个topic = skywalking-segments，partition = 9，offset = 8763770之前的消息。为了不使得代码过于复杂，我们这里就不使用兜底方案了，丢了就丢了吧。我们允许存在少量的重复数据。2022-09-14 19:13:48
            LinkedBlockingQueue<Map<TopicPartition, OffsetAndMetadata>> offsetsLinkedBlockingQueue = msKafkaSegmentsConsumer.getOffsetsLinkedBlockingQueue();
            boolean offer = offsetsLinkedBlockingQueue.offer(topicPartitionOffsetAndMetadataMap);
            if (false == offer) {
              log.error("# MingshiServerUtil.aiitCommitOffset() # 当前线程 = 【{}】将消费的partition【{}】放入到队列中失败，当前队列大小 = 【{}】，其容量 = 【{}】。", Thread.currentThread().getName(), JsonUtil.obj2String(offsetsMap), offsetsLinkedBlockingQueue.size(), gracefulShutdownQueueSize);
            }
          }
        }
        log.info("# MingshiServerUtil.aiitCommitOffset() # 当前线程 = 【{}】消费的partition有【{}】。", Thread.currentThread().getName(), JsonUtil.obj2String(offsetsMap));
      } catch (Exception e) {
        log.error("# MingshiServerUtil.aiitCommitOffset() # 当前线程 = 【{}】将消费的partition【{}】放入到队列中出现了异常。", Thread.currentThread().getName(), JsonUtil.obj2String(offsetsMap), e);
      } finally {
        offsetsMap.clear();
      }
    }
  }

  private Map<TopicPartition, OffsetAndMetadata> buildCommits(Map<String, Map<Integer, Long>> offsetsMap) {
    Map<TopicPartition, OffsetAndMetadata> commits = new HashMap<>(Const.INITAL_SIZE);
    for (Map.Entry<String, Map<Integer, Long>> entry : offsetsMap.entrySet()) {
      for (Map.Entry<Integer, Long> offset : entry.getValue().entrySet()) {
        commits.put(new TopicPartition(entry.getKey(), offset.getKey()), new OffsetAndMetadata(offset.getValue() + 1));
      }
    }
    return commits;
  }
}
