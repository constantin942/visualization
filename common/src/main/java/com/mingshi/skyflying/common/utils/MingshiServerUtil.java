package com.mingshi.skyflying.common.utils;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.protobuf.InvalidProtocolBufferException;
import com.mingshi.skyflying.common.agent.AgentInformationSingleton;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.dao.*;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.common.kafka.producer.AiitKafkaProducer;
import com.mingshi.skyflying.common.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.common.reactor.queue.IoThreadLinkedBlockingQueue;
import com.mingshi.skyflying.common.reactor.thread.IoThread;
import com.mingshi.skyflying.common.reactor.thread.ProcessorThread;
import com.mingshi.skyflying.common.sql.SqlTypeMap;
import com.mingshi.skyflying.common.statistics.InformationOverviewSingleton;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.common.utils.Bytes;
import org.apache.kafka.common.utils.CopyOnWriteMap;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentObject;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;
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
    @Value("${reactor.iothread.thread.count}")
    private Integer reactorIoThreadCount;
    @Resource
    private AiitKafkaProducer aiitKafkaProducer;
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
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private MsConfigDao msConfigDao;

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
     * <B>方法名称：getAkSk</B>
     * <B>概要说明：从数据库中获取ak、sk信息</B>
     *
     * @return com.fasterxml.jackson.databind.node.ObjectNode
     * @Author zm
     * @Date 2022年10月10日 09:10:49
     * @Param []
     **/
    public ObjectNode getAkSk(Boolean scheduled) {
        ObjectNode jsonNodes = JsonUtil.createJsonObject();
        MsConfigDo msConfigDo = msConfigDao.selectByConfigType(Const.AK_SK);
        if (null == msConfigDo || StringUtil.isBlank(msConfigDo.getConfig())) {
            if (Boolean.TRUE.equals(scheduled)) {
                log.error("#AuditLogServiceImpl.getAkSk()# 通过定时任务自动拉取DMS审计日志时，在数据库中没有获取到aksk配置。");
            }
            return null;
        }
        String config = null;
        String ak = null;
        String sk = null;
        try {
            config = msConfigDo.getConfig();

            ObjectNode jsonObject = JsonUtil.string2Object(config, ObjectNode.class);
            if (null == jsonObject.get(Const.AK) || null == jsonObject.get(Const.SK) || StringUtil.isBlank(jsonObject.get(Const.AK).asText()) || StringUtil.isBlank(jsonObject.get(Const.SK).asText())) {
                if (Boolean.TRUE.equals(scheduled)) {
                    log.error("#AuditLogServiceImpl.getAkSk()# 通过定时任务自动拉取DMS审计日志时，在数据库中没有获取到ak或者sk配置。");
                }
                return null;
            }
            ak = jsonObject.get(Const.AK).asText();
            sk = jsonObject.get(Const.SK).asText();
        } catch (Exception e) {
            if (Boolean.TRUE.equals(scheduled)) {
                log.error("#AuditLogServiceImpl.getAkSk()# 通过定时任务自动拉取DMS的审计日志，解析在数据库中获取到的aksk配置 = 【{}】时，出现了异常。", config, e);
            }
            return null;
        }
        jsonNodes.put(Const.AK, ak);
        jsonNodes.put(Const.SK, sk);
        return jsonNodes;
    }

    /**
     * <B>方法名称：recordForwarding</B>
     * <B>概要说明：将蓝景kafka服务器上的消息转发到我们内网测试环境中的kafka上</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年06月24日 09:06:35
     * @Param [record]
     **/
    public void recordForwarding(ConsumerRecord<String, Bytes> consumerRecord, String topic) {
        try {
            SegmentObject segmentObject = null;
            segmentObject = SegmentObject.parseFrom(consumerRecord.value().get());
            aiitKafkaProducer.send(segmentObject, topic);
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
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
    public void doEnableReactorModel(Map<String, Integer> statisticsProcessorThreadQpsMap,
                                     List<MsSegmentDetailDo> segmentDetaiDolList,
                                     List<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList,
                                     Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap) {
        try {
            ObjectNode jsonObject = JsonUtil.createJsonObject();
            /**
             * 统计当前线程的QPS；2022-07-23 11:05:16
             */
            if (null != statisticsProcessorThreadQpsMap && 0 < statisticsProcessorThreadQpsMap.size()) {
                jsonObject.put(Const.QPS_ZSET_EVERY_PROCESSOR_THREAD, JsonUtil.obj2String(statisticsProcessorThreadQpsMap));
            }
            if (null != segmentDetaiDolList && !segmentDetaiDolList.isEmpty()) {
                jsonObject.put(Const.SEGMENT_DETAIL_DO_LIST, JsonUtil.obj2String(segmentDetaiDolList));
            }
            if (null != segmentDetaiUserNameIsNullDolList && !segmentDetaiUserNameIsNullDolList.isEmpty()) {
                jsonObject.put(Const.SEGMENT_DETAIL_USERNAME_IS_NULL_DO_LIST, JsonUtil.obj2String(segmentDetaiUserNameIsNullDolList));
            }
            if (null != skywalkingAgentHeartBeatMap && 0 < skywalkingAgentHeartBeatMap.size()) {
                jsonObject.put(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, JsonUtil.obj2String(skywalkingAgentHeartBeatMap));
            }
            doEnableReactorModel(jsonObject, reactorIoThreadCount, mingshiServerUtil);
        } catch (Exception e) {
            log.error("将清洗好的调用链信息放入到队列中出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：doEnableReactorModel</B>
     * <B>概要说明：不使用优雅关机的方式，可以提高吞吐量，但在异常情况下，会造成消息的丢失</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年09月27日 09:09:06
     * @Param [jsonObject, gracefulShutdown, reactorIoThreadThreadCount, mingshiServerUtil]
     **/
    public void doEnableReactorModel(ObjectNode jsonObject, Integer reactorIoThreadCount, MingshiServerUtil mingshiServerUtil) {
        if (null == jsonObject || Const.NUMBER_ZERO == jsonObject.size()) {
            return;
        }
        Boolean result = false;
        while (false == result) {
            IoThread ioThread = IoThreadLinkedBlockingQueue.getLinkedBlockingQueue(reactorIoThreadCount, mingshiServerUtil);
            if (null != ioThread) {
                try {
                    result = ioThread.offer(jsonObject);
                } catch (Exception e) {
                    log.error("# MingshiServerUtil.doEnableReactorModel() # 往IoThread线程中存放数据时，出现了异常。", e);
                }
            } else {
                log.error("# MingshiServerUtil.doEnableReactorModel() # 不使用优雅关机的方式，往IoThread线程中存放数据时，没有获取到IoThread线程。");
            }
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
        String sqlTypeFromLibrary = doGetSqlTypeFromLibrary(msSql);
        if (StringUtil.isNotBlank(sqlTypeFromLibrary)) {
            return sqlTypeFromLibrary;
        }
        return doGetSqlType(msSql);
    }

    private String doGetSqlTypeFromLibrary(String msSql) {
        String sqlType = null;
        try {
            sqlType = SqlParserUtils.getSqlType(msSql);
            if (StringUtil.isNotBlank(sqlType)) {
                sqlType = sqlType.toLowerCase();
            }
        } catch (Exception e) {
            // ignore
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
        }
        if (null != tableNameList && !tableNameList.isEmpty()) {
            for (String table : tableNameList) {
                if (StringUtil.isBlank(tableName)) {
                    tableName = table;
                } else {
                    tableName = tableName + Const.POUND_KEY + table;
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
     * <B>方法名称：flushSegmentDetailCountToRedis</B>
     * <B>概要说明：实时segmentDetail数据的统计数量保存到Redis的哈希表中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月18日 16:07:28
     * @Param [count]
     **/
//    public void flushSegmentDetailCountToRedis(List<MsSegmentDetailDo> list) {
//        Instant now = Instant.now();
//        if(list.isEmpty()){
//            return;
//        }
//        try {
//            Map<String, Integer> everydayVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 用户总的访问次数；2022-10-14 13:59:31
//            Map<String, Integer> userAccessBehaviorAllVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 用户最后的访问时间；2022-10-14 14:05:16
//            Map<String, String> userAccessBehaviorLatestVisitedTimeMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 表最后的访问时间；2022-10-14 14:05:16
//            Map<String, String> tableLatestVisitedTimeMap = new HashMap<>(Const.NUMBER_EIGHT);
//
//            // 统计表每天的访问次数；2022-10-14 14:22:59
//            Map<String/* 表名 */, Map<String/* 每天的日期，格式是：yyyy-MM-dd */, Long/* 访问次数 */>> tableEverydayVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计用户访问过的表的次数；2022-10-14 14:55:50
//            Map<String/* 用户名 */, Map<String/* 表名 */, Double/* 访问次数 */>> userAccessBehaviorAllVisitedTablesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计表被用户访问过的次数；2022-10-14 15:11:17
//            Map<String/* 表名 */, Map<String/* 用户名 */, Double/* 访问次数 */>> tableByHowManyUserVisitedMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计每个表操作类型次数；2022-10-14 16:48:04
//            Map<String/* 表名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> tableOperationTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计每个用户的操作类型次数
//            Map<String/* 用户名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> userOperationTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
//
//            for (MsSegmentDetailDo msSegmentDetailDo : list) {
//                // 对各个指标进行统计；2022-10-14 17:29:03
//                doStatistics(msSegmentDetailDo,everydayVisitedTimesMap,userAccessBehaviorAllVisitedTimesMap,userAccessBehaviorLatestVisitedTimeMap,tableLatestVisitedTimeMap,tableEverydayVisitedTimesMap,userAccessBehaviorAllVisitedTablesMap,tableByHowManyUserVisitedMap,tableOperationTypeMap,userOperationTypeMap);
//            }
//
//            // 将统计好的各个指标进行统计发送到Redis中；2022-10-14 17:28:56
//            flushStatisticsToRedis(everydayVisitedTimesMap,userAccessBehaviorAllVisitedTimesMap,userAccessBehaviorLatestVisitedTimeMap,tableLatestVisitedTimeMap,tableEverydayVisitedTimesMap,userAccessBehaviorAllVisitedTablesMap,tableByHowManyUserVisitedMap,tableOperationTypeMap,userOperationTypeMap);
//
////            log.info("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时统计【{}】条segmentDetail数据到Redis的哈希表中，耗时【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
//        } catch (Exception e) {
//            log.error("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时segmentDetail数据的统计数量保存到Redis的哈希表中，出现了异常。", e);
//        }
//    }
//    public void flushSegmentDetailCountToRedis(List<MsSegmentDetailDo> list) {
//        Instant now = Instant.now();
//        if(list.isEmpty()){
//            return;
//        }
//        try {
//            Map<String, Integer> everydayVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 用户总的访问次数；2022-10-14 13:59:31
//            Map<String, Integer> userAccessBehaviorAllVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 用户最后的访问时间；2022-10-14 14:05:16
//            Map<String, String> userAccessBehaviorLatestVisitedTimeMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 表最后的访问时间；2022-10-14 14:05:16
//            Map<String, String> tableLatestVisitedTimeMap = new HashMap<>(Const.NUMBER_EIGHT);
//
//            // 统计表每天的访问次数；2022-10-14 14:22:59
//            Map<String/* 表名 */, Map<String/* 每天的日期，格式是：yyyy-MM-dd */, Long/* 访问次数 */>> tableEverydayVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计用户访问过的表的次数；2022-10-14 14:55:50
//            Map<String/* 用户名 */, Map<String/* 表名 */, Double/* 访问次数 */>> userAccessBehaviorAllVisitedTablesMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计表被用户访问过的次数；2022-10-14 15:11:17
//            Map<String/* 表名 */, Map<String/* 用户名 */, Double/* 访问次数 */>> tableByHowManyUserVisitedMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计每个表操作类型次数；2022-10-14 16:48:04
//            Map<String/* 表名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> tableOperationTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
//            // 统计每个用户的操作类型次数
//            Map<String/* 用户名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> userOperationTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
//
//            for (MsSegmentDetailDo msSegmentDetailDo : list) {
//                // 对各个指标进行统计；2022-10-14 17:29:03
//                doStatistics(msSegmentDetailDo,everydayVisitedTimesMap,userAccessBehaviorAllVisitedTimesMap,userAccessBehaviorLatestVisitedTimeMap,tableLatestVisitedTimeMap,tableEverydayVisitedTimesMap,userAccessBehaviorAllVisitedTablesMap,tableByHowManyUserVisitedMap,tableOperationTypeMap,userOperationTypeMap);
//            }
//
//            // 将统计好的各个指标进行统计发送到Redis中；2022-10-14 17:28:56
//            flushToRedis(everydayVisitedTimesMap,userAccessBehaviorAllVisitedTimesMap,userAccessBehaviorLatestVisitedTimeMap,tableLatestVisitedTimeMap,tableEverydayVisitedTimesMap,userAccessBehaviorAllVisitedTablesMap,tableByHowManyUserVisitedMap,tableOperationTypeMap,userOperationTypeMap);
//
////            log.info("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时统计【{}】条segmentDetail数据到Redis的哈希表中，耗时【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
//        } catch (Exception e) {
//            log.error("# MingshiServerUtil.flushSegmentDetailCountToRedis() # 实时segmentDetail数据的统计数量保存到Redis的哈希表中，出现了异常。", e);
//        }
//    }

    /**
     * <B>方法名称：flushStatisticsToRedis</B>
     * <B>概要说明：将统计好的各个指标进行统计发送到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 17:29:32
     * @Param [everydayVisitedTimesMap, userAccessBehaviorAllVisitedTimesMap, userAccessBehaviorLatestVisitedTimeMap, tableLatestVisitedTimeMap, tableEverydayVisitedTimesMap, userAccessBehaviorAllVisitedTablesMap, tableByHowManyUserVisitedMap, tableOperationTypeMap, userOperationTypeMap]
     **/
    public void flushStatisticsToRedis(Map<String, Integer> everydayVisitedTimesMap, Map<String, Integer> userAccessBehaviorAllVisitedTimesMap, Map<String, String> userAccessBehaviorLatestVisitedTimeMap, Map<String, String> tableLatestVisitedTimeMap, Map<String, Map<String, Long>> tableEverydayVisitedTimesMap, Map<String, Map<String, Double>> userAccessBehaviorAllVisitedTablesMap, Map<String, Map<String, Double>> tableByHowManyUserVisitedMap, Map<String, Map<String, Double>> tableOperationTypeMap, Map<String, Map<String, Double>> userOperationTypeMap) {
        // 将用户的最后访问时间更新到Redis中
        flushUserAccessBehaviorLatestVisitedTimeToRedis(userAccessBehaviorLatestVisitedTimeMap);

        // 将用户的访问次数累加到Redis中
        flushUserAccessBehaviorAllVisitedTimesToRedis(userAccessBehaviorAllVisitedTimesMap);

        // 将表每天的访问次数发送到Redis中
        flushTableEverydayVisitedTimes(tableEverydayVisitedTimesMap);

        // 将条最后的访问时间更新到Redis中；
        flushTableLatestVisitiedTime(tableLatestVisitedTimeMap);

        // 更新每天采集情况和总的采集情况到Redis；2022-07-20 14:17:03
        updateEverydayStatisticToRedis(everydayVisitedTimesMap);

        // 将用户访问过的表的次数发送到Redis中；
        flushUserAccessBehaviorAllVisitedTables(userAccessBehaviorAllVisitedTablesMap);

        // 将表被用户访问的次数发送Redis中；
        flushTableByHowManyUserVisited(tableByHowManyUserVisitedMap);

        // 将每个表的操作类型次数发送到Redis中；
        flushTableOperationType(tableOperationTypeMap);

        // 统计每个用户操作类型次数；
        flushUserOperationType(userOperationTypeMap);
    }

    /**
     * <B>方法名称：doStatistics</B>
     * <B>概要说明：对各个指标进行统计</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 17:27:55
     * @Param [msSegmentDetailDo, everydayVisitedTimesMap, userAccessBehaviorAllVisitedTimesMap, userAccessBehaviorLatestVisitedTimeMap, tableLatestVisitedTimeMap, tableEverydayVisitedTimesMap, userAccessBehaviorAllVisitedTablesMap, tableByHowManyUserVisitedMap, tableOperationTypeMap, userOperationTypeMap]
     **/
    public void doStatistics(MsSegmentDetailDo msSegmentDetailDo, Map<String, Integer> everydayVisitedTimesMap, Map<String, Integer> userAccessBehaviorAllVisitedTimesMap, Map<String, String> userAccessBehaviorLatestVisitedTimeMap, Map<String, String> tableLatestVisitedTimeMap, Map<String, Map<String, Long>> tableEverydayVisitedTimesMap, Map<String, Map<String, Double>> userAccessBehaviorAllVisitedTablesMap, Map<String, Map<String, Double>> tableByHowManyUserVisitedMap, Map<String, Map<String, Double>> tableOperationTypeMap, Map<String, Map<String, Double>> userOperationTypeMap) {
        String userName = msSegmentDetailDo.getUserName();
        String startTime = msSegmentDetailDo.getStartTime();
        String tableName = msSegmentDetailDo.getMsTableName();
        String dbInstance = msSegmentDetailDo.getDbInstance();
        String dbType = msSegmentDetailDo.getDbType();
        String peer = msSegmentDetailDo.getPeer();
        String serviceCode = msSegmentDetailDo.getServiceCode();
        if (StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(peer) && StringUtil.isNotBlank(dbInstance) && StringUtil.isNotBlank(tableName)) {

            // 对用户总的访问次数进行本地累加
            userAccessBehaviorAllVisitedTimes(userName, userAccessBehaviorAllVisitedTimesMap);

            // 更新用户的最后访问时间
            userAccessBehaviorLatestVisitedTime(userName, startTime, userAccessBehaviorLatestVisitedTimeMap);

            // 将表每天的访问次数在本地进行累加
            tableEverydayVisitedTimes(tableName, peer, dbInstance, startTime, tableEverydayVisitedTimesMap);

            // 更新表最后的访问时间
            tableLatestVisitedTime(peer, dbInstance, tableName, startTime, tableLatestVisitedTimeMap);

            // 统计用户访问过的表的次数；
            userAccessBehaviorAllVisitedTables(peer, dbInstance, tableName, userName, userAccessBehaviorAllVisitedTablesMap);

            // 统计表被用户访问的次数；
            tableByHowManyUserVisited(peer, dbInstance, tableName, userName, serviceCode, tableByHowManyUserVisitedMap);

            // 统计每个表操作类型次数；
            tableOperationType(dbType, peer, dbInstance, tableName, tableOperationTypeMap);

            // 统计每个用户操作类型次数；
            userOperationType(dbType, userName, userOperationTypeMap);

            // 实时将用户访问行为信息发送到redis中
            setTableEnableStatus(peer, dbInstance, tableName);

            // 根据年月日，统计每天的访问次数；2022-07-20 14:11:55
            statisticVisitedCountByEveryday(msSegmentDetailDo, everydayVisitedTimesMap);
        }
    }

    /**
     * <B>方法名称：flushUserOperationType</B>
     * <B>概要说明：统计每个用户操作类型次数；</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 17:22:44
     * @Param [userOperationTypeMap]
     **/
    private void flushUserOperationType(Map<String, Map<String, Double>> userOperationTypeMap) {
        if (null == userOperationTypeMap || userOperationTypeMap.isEmpty()) {
            return;
        }
        try {
            Iterator<String> iterator = userOperationTypeMap.keySet().iterator();
            while (iterator.hasNext()) {
                String userName = iterator.next();
                Map<String, Double> operationTypeCountMap = userOperationTypeMap.get(userName);
                if (null == operationTypeCountMap || operationTypeCountMap.isEmpty()) {
                    continue;
                }
                Iterator<String> iterator1 = operationTypeCountMap.keySet().iterator();
                while (iterator1.hasNext()) {
                    String operationType = iterator1.next();
                    Double count = operationTypeCountMap.get(operationType);
                    if (null == count) {
                        continue;
                    }
                    // 有序集合：统计每个用户操作类型次数；
                    redisPoolUtil.zSetIncrementScore(userName, operationType, count);
                }
            }
            userOperationTypeMap.clear();
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushUserOperationType() # 将统计每个用户操作类型次数发送到Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：userOperationType</B>
     * <B>概要说明：统计每个用户操作类型次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 17:16:15
     * @Param [dbType, userName, userOperationTypeMap]
     **/
    private void userOperationType(String dbType, String userName, Map<String, Map<String, Double>> userOperationTypeMap) {
        String key = Const.ZSET_USER_OPERATION_TYPE + userName;
        Map<String, Double> operationTypeCountMap = userOperationTypeMap.get(key);
        if (null == operationTypeCountMap) {
            operationTypeCountMap = new HashMap<>();
            operationTypeCountMap.put(dbType, 1d);
            userOperationTypeMap.put(key, operationTypeCountMap);
        } else {
            Double count = operationTypeCountMap.get(dbType);
            operationTypeCountMap.put(dbType, null == count ? 1 : count + 1);
        }
    }

    /**
     * <B>方法名称：flushTableOperationType</B>
     * <B>概要说明：对每个表的操作类型次数发送到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 17:00:34
     * @Param [userOperationTypeMap]
     **/
    private void flushTableOperationType(Map<String, Map<String, Double>> userOperationTypeMap) {
        try {
            if (null == userOperationTypeMap || userOperationTypeMap.isEmpty()) {
                return;
            }
            Iterator<String> iterator = userOperationTypeMap.keySet().iterator();
            while (iterator.hasNext()) {
                String tableName = iterator.next();
                Map<String, Double> operationTypeMap = userOperationTypeMap.get(tableName);
                if (null == operationTypeMap || operationTypeMap.isEmpty()) {
                    continue;
                }
                Iterator<String> iterator1 = operationTypeMap.keySet().iterator();
                while (iterator1.hasNext()) {
                    String operationType = iterator1.next();
                    Double count = operationTypeMap.get(operationType);
                    if (null == count || 0 >= count) {
                        continue;
                    }
                    // 有序集合：存放对每个表操作类型统计；2022-07-22 15:47:48
                    redisPoolUtil.zSetIncrementScore(tableName, operationType, count);
                }
            }
            userOperationTypeMap.clear();
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushTableOperationType() # 将对每个表的操作类型次数发送到Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：tableOperationType</B>
     * <B>概要说明：本地暂存每个表的操作类型次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 17:02:22
     * @Param [dbType, peer, dbInstance, tableName, userOperationTypeMap]
     **/
    private void tableOperationType(String dbType, String peer, String dbInstance, String tableName, Map<String/* 表名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> tableOperationTypeMap) {
        try {
            // 累加用户对数据库表资源的访问次数；
            String zsetVlue = doGetTableName(peer, dbInstance, tableName);
            if (tableName.contains(Const.EN_COMMA)) {
                String[] split = tableName.split(Const.EN_COMMA);
                for (String tn : split) {
                    // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
                    zsetVlue = doGetTableName(peer, dbInstance, tn);
                    doTableOperationType(zsetVlue, dbType, tableOperationTypeMap);
                }
            } else {
                doTableOperationType(zsetVlue, dbType, tableOperationTypeMap);
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.userOperationType() # 本地暂存每个表的操作类型次数，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：doUserOperationType</B>
     * <B>概要说明：统计对每个表的操作类型</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 16:57:53
     * @Param [zsetVlue, dbType, userOperationTypeMap]
     **/
    private void doTableOperationType(String zsetVlue, String dbType, Map<String, Map<String, Double>> tableOperationTypeMap) {
        // 有序集合：存放对每个表操作类型统计；2022-07-22 15:47:48
        String key = Const.ZSET_TABLE_OPERATION_TYPE + zsetVlue;
        Map<String, Double> operationTypeMap = tableOperationTypeMap.get(key);
        if (null == operationTypeMap) {
            operationTypeMap = new HashMap<>();
            operationTypeMap.put(dbType, 1d);
            tableOperationTypeMap.put(key, operationTypeMap);
        } else {
            Double count = operationTypeMap.get(dbType);
            operationTypeMap.put(dbType, null == count ? 1d : count + 1);
        }
    }

    /**
     * <B>方法名称：flushTableByHowManyUserVisited</B>
     * <B>概要说明：将表被用户访问的次数发送Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 15:24:17
     * @Param [tableByHowManyUserVisitedMap]
     **/
    private void flushTableByHowManyUserVisited(Map<String, Map<String, Double>> tableByHowManyUserVisitedMap) {
        if (null == tableByHowManyUserVisitedMap || tableByHowManyUserVisitedMap.isEmpty()) {
            return;
        }
        try {
            Iterator<String> iterator = tableByHowManyUserVisitedMap.keySet().iterator();
            while (iterator.hasNext()) {
                String tableName = iterator.next();
                Map<String, Double> userNameVisitedTimeMap = tableByHowManyUserVisitedMap.get(tableName);
                if (null == userNameVisitedTimeMap || userNameVisitedTimeMap.isEmpty()) {
                    continue;
                }
                Iterator<String> iterator1 = userNameVisitedTimeMap.keySet().iterator();
                while (iterator1.hasNext()) {
                    String userName = iterator1.next();
                    Double count = userNameVisitedTimeMap.get(userName);
                    if (null == count) {
                        continue;
                    }
                    redisPoolUtil.zSetIncrementScore(tableName, userName, count);
                }
            }
            tableByHowManyUserVisitedMap.clear();
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushTableByHowManyUserVisited() # 将表被用户访问的次数发送Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：tableByHowManyUserVisited</B>
     * <B>概要说明：统计表被用户访问的次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 15:12:29
     * @Param [dbType, peer, dbInstance, tableName, userName, startTime, serviceCode, tableByHowManyUserVisitedMap]
     **/
    private void tableByHowManyUserVisited(String peer, String dbInstance, String tableName, String userName, String serviceCode, Map<String, Map<String, Double>> tableByHowManyUserVisitedMap) {
        // 累加用户对数据库表资源的访问次数；
        String zsetVlue = doGetTableName(peer, dbInstance, tableName);
        String serviceCodeName = AgentInformationSingleton.get(serviceCode);
        if (StringUtil.isNotBlank(serviceCodeName)) {
            serviceCode = serviceCodeName.equals(Const.DOLLAR) == true ? serviceCode : serviceCodeName;
        }

        if (tableName.contains(Const.EN_COMMA)) {
            String[] split = tableName.split(Const.EN_COMMA);
            for (String tn : split) {
                // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
                zsetVlue = doGetTableName(peer, dbInstance, tn);
                // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
                doTableByHowManyUserVisited(zsetVlue, userName, serviceCode, tableByHowManyUserVisitedMap);
            }
        } else {
            // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
            doTableByHowManyUserVisited(zsetVlue, userName, serviceCode, tableByHowManyUserVisitedMap);
        }
    }

    private void doTableByHowManyUserVisited(String zsetVlue, String userName, String serviceCode, Map<String, Map<String, Double>> tableByHowManyUserVisitedMap) {
        String tableName = Const.ZSET_TABLE_BY_HOW_MANY_USER_VISITED + zsetVlue;
        String userName1 = serviceCode + Const.DOLLAR + userName;

        Map<String, Double> userNameVisitedTimeMap = tableByHowManyUserVisitedMap.get(tableName);
        if (null == userNameVisitedTimeMap) {
            userNameVisitedTimeMap = new HashMap<>();
            userNameVisitedTimeMap.put(userName, 1d);
            tableByHowManyUserVisitedMap.put(tableName, userNameVisitedTimeMap);
        } else {
            Double count = userNameVisitedTimeMap.get(userName1);
            userNameVisitedTimeMap.put(userName1, null == count ? 1 : count + 1);
        }
    }

    /**
     * <B>方法名称：flushUserAccessBehaviorAllVisitedTables</B>
     * <B>概要说明：将本地统计用户访问过的表的次数发送到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 15:09:11
     * @Param [userAccessBehaviorAllVisitedTablesMap]
     **/
    private void flushUserAccessBehaviorAllVisitedTables(Map<String, Map<String, Double>> userAccessBehaviorAllVisitedTablesMap) {
        try {
            if (null == userAccessBehaviorAllVisitedTablesMap || userAccessBehaviorAllVisitedTablesMap.isEmpty()) {
                return;
            }
            Iterator<String> iterator = userAccessBehaviorAllVisitedTablesMap.keySet().iterator();
            while (iterator.hasNext()) {
                String userName = iterator.next();
                Map<String, Double> tableCountMap = userAccessBehaviorAllVisitedTablesMap.get(userName);
                if (null == tableCountMap || tableCountMap.isEmpty()) {
                    continue;
                }
                Iterator<String> iterator1 = tableCountMap.keySet().iterator();
                while (iterator1.hasNext()) {
                    String tableName = iterator1.next();
                    Double count = tableCountMap.get(tableName);
                    redisPoolUtil.zSetIncrementScore(userName, tableName, null == count ? 1 : count);
                }
            }
            userAccessBehaviorAllVisitedTablesMap.clear();
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushUserAccessBehaviorAllVisitedTables() # 将本地统计用户访问过的表的次数发送到Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：userAccessBehaviorAllVisitedTables</B>
     * <B>概要说明：统计用户访问过的表的次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:58:16
     * @Param [peer, dbInstance, tableName, userName, userAccessBehaviorAllVisitedTablesMap]
     **/
    private void userAccessBehaviorAllVisitedTables(String peer, String dbInstance, String tableName, String userName, Map<String, Map<String, Double>> userAccessBehaviorAllVisitedTablesMap) {
        // 累加用户对数据库表资源的访问次数；
        try {
            String zsetVlue = doGetTableName(peer, dbInstance, tableName);
            if (tableName.contains(Const.EN_COMMA)) {
                String[] split = tableName.split(Const.EN_COMMA);
                for (String tn : split) {
                    // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
                    zsetVlue = doGetTableName(peer, dbInstance, tn);
                    // 将用户访问过的表放到这个用户对应的有序集合zset中；2022-07-20 14:30:07
                    String key = Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName;
                    doUserAccessBehaviorAllVisitedTables(zsetVlue, key, userAccessBehaviorAllVisitedTablesMap);
                }
            } else {
                // 将用户访问过的表放到这个用户对应的有序集合zset中；2022-07-20 14:30:07
                String key = Const.ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES + userName;
                doUserAccessBehaviorAllVisitedTables(zsetVlue, key, userAccessBehaviorAllVisitedTablesMap);
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.userAccessBehaviorAllVisitedTables() # 在本地统计用户访问过的表的次数时，出现了异常。", e);
        }
    }

    private void doUserAccessBehaviorAllVisitedTables(String zsetVlue, String key, Map<String, Map<String, Double>> userAccessBehaviorAllVisitedTablesMap) {
        Map<String, Double> tableVisitedTimesMap = userAccessBehaviorAllVisitedTablesMap.get(key);
        if (null == tableVisitedTimesMap) {
            tableVisitedTimesMap = new HashMap<>();
            tableVisitedTimesMap.put(zsetVlue, 1d);
            userAccessBehaviorAllVisitedTablesMap.put(key, tableVisitedTimesMap);
        } else {
            Double count = tableVisitedTimesMap.get(zsetVlue);
            tableVisitedTimesMap.put(zsetVlue, null == count ? 1d : count + 1);
        }
    }

    /**
     * <B>方法名称：flushTableLatestVisitiedTime</B>
     * <B>概要说明：将表的最后访问时间更新到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:53:34
     * @Param [tableLatestVisitedTimeMap]
     **/
    private void flushTableLatestVisitiedTime(Map<String, String> tableLatestVisitedTimeMap) {
        try {
            if (null == tableLatestVisitedTimeMap || tableLatestVisitedTimeMap.isEmpty()) {
                return;
            }
            Iterator<String> iterator = tableLatestVisitedTimeMap.keySet().iterator();
            while (iterator.hasNext()) {
                String tableName = iterator.next();
                String time = tableLatestVisitedTimeMap.get(tableName);
                if (StringUtil.isBlank(time)) {
                    continue;
                }
                redisPoolUtil.set(tableName, time);
            }
            tableLatestVisitedTimeMap.clear();
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushTableLatestVisitiedTime() # 将表的最后访问时间更新到Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：tableLatestVisitedTime</B>
     * <B>概要说明：更新表最后的访问时间</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:48:12
     * @Param [peer, dbInstance, tableName, startTime, tableLatestVisitedTimeMap]
     **/
    private void tableLatestVisitedTime(String peer, String dbInstance, String tableName, String startTime, Map<String, String> tableLatestVisitedTimeMap) {
        String zsetVlue = doGetTableName(peer, dbInstance, tableName);

        if (null == tableLatestVisitedTimeMap) {
            tableLatestVisitedTimeMap = new HashMap<>();
        }

        if (tableName.contains(Const.EN_COMMA)) {
            String[] split = tableName.split(Const.EN_COMMA);
            for (String tn : split) {
                // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
                zsetVlue = doGetTableName(peer, dbInstance, tn);
                // 记录每一个数据库表最后的被访问的时间；
                String key = Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue;
                tableLatestVisitedTimeMap.put(key, startTime);
            }
        } else {
            // 记录每一个数据库表最后的被访问的时间；
            String key = Const.STRING_TABLE_LATEST_VISITED_TIME + zsetVlue;
            tableLatestVisitedTimeMap.put(key, startTime);
        }
    }

    /**
     * <B>方法名称：flushTableEverydayVisitedTimes</B>
     * <B>概要说明：将表每天的访问次数发送到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:39:52
     * @Param [tableEverydayVisitedTimesMap]
     **/
    private void flushTableEverydayVisitedTimes(Map<String, Map<String, Long>> tableEverydayVisitedTimesMap) {
        try {
            if (null == tableEverydayVisitedTimesMap || tableEverydayVisitedTimesMap.isEmpty()) {
                return;
            }
            Iterator<String> iterator = tableEverydayVisitedTimesMap.keySet().iterator();
            while (iterator.hasNext()) {
                String tableName = iterator.next();
                Map<String, Long> timeTimesMap = tableEverydayVisitedTimesMap.get(tableName);
                if (null != timeTimesMap && !timeTimesMap.isEmpty()) {
                    Iterator<String> iterator1 = timeTimesMap.keySet().iterator();
                    while (iterator1.hasNext()) {
                        String time = iterator1.next();
                        Long count = timeTimesMap.get(time);
                        redisPoolUtil.hsetIncrBy(tableName, time, null == count ? 1L : count);
                    }
                }
            }
            tableEverydayVisitedTimesMap.clear();
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushTableEverydayVisitedTimes() # 将表每天的访问次数发送到Redis中，出现了异常.", e);
        }
    }

    private void tableEverydayVisitedTimes(String tableName, String peer, String dbInstance, String startTime, Map<String, Map<String, Long>> tableEverydayVisitedTimesMap) {
        // 累加用户对数据库表资源的访问次数；
        String zsetVlue = doGetTableName(peer, dbInstance, tableName);
        Date date = DateTimeUtil.strToDate(startTime);
        String startTimeNew = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);

        if (tableName.contains(Const.EN_COMMA)) {
            String[] split = tableName.split(Const.EN_COMMA);
            for (String tn : split) {
                // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
                zsetVlue = doGetTableName(peer, dbInstance, tn);
                String key = Const.HASH_TABLE_EVERYDAY_VISITED_TIMES + zsetVlue;
                doTableEverydayVisitedTimes(key, startTimeNew, tableEverydayVisitedTimesMap);
            }
        } else {
            // 对每一个表，统计每天的访问次数；2022-07-22 10:42:33
            String key = Const.HASH_TABLE_EVERYDAY_VISITED_TIMES + zsetVlue;
            doTableEverydayVisitedTimes(key, startTimeNew, tableEverydayVisitedTimesMap);
        }
    }

    private void doTableEverydayVisitedTimes(String key, String startTime, Map<String, Map<String, Long>> tableEverydayVisitedTimesMap) {
        // 对每一个表，统计每天的访问次数；2022-07-22 10:42:33
        if (null == tableEverydayVisitedTimesMap) {
            tableEverydayVisitedTimesMap = new HashMap<>();
        }
        Map<String, Long> timeTimesMap = tableEverydayVisitedTimesMap.get(key);
        if (null == timeTimesMap) {
            timeTimesMap = new HashMap<>();
            timeTimesMap.put(startTime, 1L);
            tableEverydayVisitedTimesMap.put(key, timeTimesMap);
        } else {
            Long count = timeTimesMap.get(startTime);
            if (null == count) {
                timeTimesMap.put(startTime, 1L);
            } else {
                timeTimesMap.put(startTime, count + 1L);
            }
        }
    }

    /**
     * <B>方法名称：userAccessBehaviorLatestVisitedTime</B>
     * <B>概要说明：更新用户的最后访问时间</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:08:18
     * @Param [userName, startTime, userAccessBehaviorLatestVisitedTimeMap]
     **/
    private void userAccessBehaviorLatestVisitedTime(String userName, String startTime, Map<String, String> userAccessBehaviorLatestVisitedTimeMap) {
        if (null == userAccessBehaviorLatestVisitedTimeMap) {
            userAccessBehaviorLatestVisitedTimeMap = new HashMap<>();
        }
        userAccessBehaviorLatestVisitedTimeMap.put(Const.STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME + userName, startTime);
    }

    /**
     * <B>方法名称：userAccessBehaviorAllVisitedTimes</B>
     * <B>概要说明：对用户总的访问次数进行本地累加</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:03:34
     * @Param [userName, userAccessBehaviorAllVisitedTimesMap]
     **/
    private void userAccessBehaviorAllVisitedTimes(String userName, Map<String, Integer> userAccessBehaviorAllVisitedTimesMap) {
        if (null == userAccessBehaviorAllVisitedTimesMap) {
            userAccessBehaviorAllVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
        }
        // 用户访问次数 + 1；
        String key = Const.STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES + userName;
        Integer count = userAccessBehaviorAllVisitedTimesMap.get(key);
        if (null != count) {
            userAccessBehaviorAllVisitedTimesMap.put(key, count + 1);
        } else {
            userAccessBehaviorAllVisitedTimesMap.put(key, 1);
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
    private void updateEverydayStatisticToRedis(Map<String, Integer> everydayVisitedTimesMap) {
        try {
            Iterator<String> iterator = everydayVisitedTimesMap.keySet().iterator();
            while (iterator.hasNext()) {
                String key = iterator.next();
                Integer value = everydayVisitedTimesMap.get(key);
                // 更新每天采集情况；
                redisPoolUtil.hsetIncrBy(Const.HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS, key, value.longValue());
                // 更新总的采集情况；
                redisPoolUtil.incr(Const.STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS, value.longValue());
            }
            everydayVisitedTimesMap.clear();
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
    public void flushUserNameToRedis(Set<String> userHashSet) {
        if (!userHashSet.isEmpty()) {
            Integer count = userHashSet.size();
            try {
                Instant now = Instant.now();
                for (String userName : userHashSet) {
                    redisPoolUtil.sadd(Const.SET_DATA_STATISTICS_HOW_MANY_USERS, userName);
                    // 将用户名放到本地内存中；2022-07-19 10:12:13
                    InformationOverviewSingleton.put(userName);
                }
                log.info("# MingshiServerUtil.flushUserNameToRedis() # 实时统计将【{}】条用户名【{}】发送到redis中，耗时【{}】毫秒。", count, JsonUtil.obj2String(userHashSet), DateTimeUtil.getTimeMillis(now));
                userHashSet.clear();
            } catch (Exception e) {
                log.error("# MingshiServerUtil.flushUserNameToRedis() # 实时将用户名发送到redis中，出现了异常。", e);
            }
        }
    }

    /**
     * <B>方法名称：flushUserAccessBehaviorLatestVisitedTimeToRedis</B>
     * <B>概要说明：将用户的最后访问时间更新到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:14:16
     * @Param [userAccessBehaviorLatestVisitedTimeMap]
     **/
    private void flushUserAccessBehaviorLatestVisitedTimeToRedis(Map<String, String> userAccessBehaviorLatestVisitedTimeMap) {
        try {
            // 更新用户对数据库最后的访问时间；
            if (null != userAccessBehaviorLatestVisitedTimeMap && !userAccessBehaviorLatestVisitedTimeMap.isEmpty()) {
                Iterator<String> iterator = userAccessBehaviorLatestVisitedTimeMap.keySet().iterator();
                while (iterator.hasNext()) {
                    String key = iterator.next();
                    String value = userAccessBehaviorLatestVisitedTimeMap.get(key);
                    redisPoolUtil.set(key, value);
                }
                userAccessBehaviorLatestVisitedTimeMap.clear();
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushUserAccessBehaviorLatestVisitedTimeToRedis() # 将用户的最后访问时间更新到Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：flushUserAccessBehaviorAllVisitedTimesToRedis</B>
     * <B>概要说明：将用户的访问次数累加到Redis中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-14 14:14:40
     * @Param [userAccessBehaviorLatestVisitedTimeMap, userAccessBehaviorAllVisitedTimesMap]
     **/
    private void flushUserAccessBehaviorAllVisitedTimesToRedis(Map<String, Integer> userAccessBehaviorAllVisitedTimesMap) {
        try {
            if (null != userAccessBehaviorAllVisitedTimesMap && !userAccessBehaviorAllVisitedTimesMap.isEmpty()) {
                Iterator<String> iterator = userAccessBehaviorAllVisitedTimesMap.keySet().iterator();
                while (iterator.hasNext()) {
                    String key = iterator.next();
                    Integer value = userAccessBehaviorAllVisitedTimesMap.get(key);
                    redisPoolUtil.incr(key, null == value ? 1 : value);
                }
                userAccessBehaviorAllVisitedTimesMap.clear();
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushUserAccessBehaviorAllVisitedTimesToRedis() # 将用户的访问次数累加到Redis中，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：setTableEnableStatus</B>
     * <B>概要说明：设置表的启用状态</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月20日 17:07:45
     * @Param [peer, dbInstance, tableName, userName, startTime]
     **/
    private void setTableEnableStatus(String peer, String dbInstance, String tableName) {
        // 累加用户对数据库表资源的访问次数；
        String zsetVlue = doGetTableName(peer, dbInstance, tableName);

        if (tableName.contains(Const.EN_COMMA)) {
            String[] split = tableName.split(Const.EN_COMMA);
            for (String tn : split) {
                // 将表信息保存到Redis中；0：表示接收处理操作这个表的数据；1：表示拒绝处理操作这个表的数据；
                zsetVlue = doGetTableName(peer, dbInstance, tn);
                LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(zsetVlue);
            }
        } else {
            LoadAllEnableMonitorTablesFromDb.getTableEnableStatus(zsetVlue);
        }
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
        if (!msAlarmInformationDoLinkedListist.isEmpty()) {
            try {
                Instant now = Instant.now();
                msAlarmInformationMapper.insertSelectiveBatch(msAlarmInformationDoLinkedListist);
                log.info("#MingshiServerUtil.flushAbnormalToDB()# 将异常信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", msAlarmInformationDoLinkedListist.size(), DateTimeUtil.getTimeMillis(now));
            } catch (Exception e) {
                log.error("# MingshiServerUtil.flushAbnormalToDB() # 将异常信息批量插入到MySQL中出现了异常。", e);
            } finally {
                msAlarmInformationDoLinkedListist.clear();
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
            if (false == atomicBoolean.get()) {
                // 只有当数据有变动时，才将其刷入到数据库中；2022-06-28 17:35:54
                return;
            }
            CopyOnWriteMap<String, String> instance = AgentInformationSingleton.getInstance();
            if (null != instance && 0 < instance.size()) {
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
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushSkywalkingAgentInformationToDb() # 将探针名称信息批量插入到MySQL数据库中出现了异常。", e);
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
        if (null == skywalkingAgentTimeMap || skywalkingAgentTimeMap.isEmpty()) {
            return;
        }

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

            redisPoolUtil.hsetBatch(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST, skywalkingAgentTimeMap);
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushSkywalkingAgentNameToRedis() # 将探针名称信息批量插入到Redis中出现了异常。", e);
        } finally {
            skywalkingAgentTimeMap.clear();
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
    public void flushSegmentDetailToDb(List<MsSegmentDetailDo> segmentDetailDoList) {
        if (segmentDetailDoList.isEmpty()) {
            return;
        }

        Instant now = Instant.now();
        try {
            msSegmentDetailDao.insertSelectiveBatch(segmentDetailDoList);
            log.info("#MingshiServerUtil.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到MySQL中耗时【{}】毫秒。", segmentDetailDoList.size(), DateTimeUtil.getTimeMillis(now));
        } catch (Exception e) {
            log.error("#MingshiServerUtil.flushSegmentDetailToDB()# 将segmentDetail实例信息【{}条】批量插入到MySQL中出现了异常。", segmentDetailDoList.size(), e);
        } finally {
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
    public void flushSegmentDetailUserNameIsNullToDb(List<MsSegmentDetailDo> segmentDetaiUserNameIsNullDolList) {
        try {
            if (!segmentDetaiUserNameIsNullDolList.isEmpty()) {
                Instant now = Instant.now();
                msSegmentDetailUsernameIsNullMapper.insertSelectiveBatch(segmentDetaiUserNameIsNullDolList);
                log.info("# MingshiServerUtil.flushSegmentDetailUserNameIsNullToDb() # 将没有用户名的链路信息【{}条】插入到表中用时【{}】毫秒。 ", segmentDetaiUserNameIsNullDolList.size(), DateTimeUtil.getTimeMillis(now));
                segmentDetaiUserNameIsNullDolList.clear();
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.flushSegmentDetailUserNameIsNullToDb() # 将用户名为空的记录插入到表中出现了异常。", e);
        } finally {
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
                if (!list.isEmpty()) {
                    msMonitorBusinessSystemTablesMapper.insertSelectiveBatch(list);
                }
                isChangedMap.clear();
            }
        } catch (Exception e) {
            log.error("# MingshiServerUtil.insertMonitorTables() # 将监管表中不存在的表插入到监管表中，出现了异常。", e);
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
    public void flushProcessorThreadQpsToRedis(Map<String, Integer> processorThreadQpsMap) {
        try {
            if (null == processorThreadQpsMap || 0 == processorThreadQpsMap.size()) {
                return;
            }
            Iterator<String> iterator = processorThreadQpsMap.keySet().iterator();
            while (iterator.hasNext()) {
                String time = iterator.next();
                Integer count = processorThreadQpsMap.get(time);
                if (null != count) {
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
        if (true != reactorProcessorEnable) {
            return;
        }
        String name = Thread.currentThread().getName();
        String key = DateTimeUtil.dateToStrformat(new Date()) + "-" + name;
        List<ProcessorThread> processorThreadList = InitProcessorByLinkedBlockingQueue.getProcessorHandlerByLinkedBlockingQueueList();
        if (!processorThreadList.isEmpty()) {
            for (int i = 0; i < processorThreadList.size(); i++) {
                ProcessorThread processorThread = processorThreadList.get(i);
                Integer queueSize = processorThread.getQueueSize();
                if (0 < queueSize) {
                    redisPoolUtil.zSetIncrementScore(Const.FIRST_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE + processorThread.getName(), key, Double.valueOf(queueSize));
                }
            }
        }

        List<IoThread> ioThreadList = IoThreadLinkedBlockingQueue.getLinkedBlockingQueueList();
        if (!ioThreadList.isEmpty()) {
            for (int i = 0; i < ioThreadList.size(); i++) {
                IoThread ioThread = ioThreadList.get(i);
                Integer size = ioThread.getQueueSize();
                if (0 < size) {
                    redisPoolUtil.zSetIncrementScore(Const.SECOND_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE + ioThread.getName(), key, size.doubleValue());
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
    public void doInsertSegmentDetailIntoMySqlAndRedis(Set<String> userHashSet,
                                                       Map<String, Integer> processorThreadQpsMap,
                                                       Map<String, String> skywalkingAgentHeartBeatMap,
                                                       List<MsSegmentDetailDo> segmentDetailDoList,
                                                       List<MsSegmentDetailDo> segmentDetailUserNameIsNullDoList) {
        // 将用户名发送到redis中
        flushUserNameToRedis(userHashSet);

        // 将processor线程的QPS发送到Redis中；2022-07-23 11:22:13
        flushProcessorThreadQpsToRedis(processorThreadQpsMap);

        // 将公共队列中有多少元素没有被消费发送到Redis中统计，便于日常的系统调优；2022-07-23 11:33:39
        statisticsProcessorAndIoThreadQueueSize();

        // 将探针信息刷入MySQL数据库中；2022-06-27 13:42:13
        flushSkywalkingAgentInformationToDb();

        // 将探针名称发送到Redis中，用于心跳检测；2022-06-27 13:42:13
        flushSkywalkingAgentNameToRedis(skywalkingAgentHeartBeatMap);

        // 将业务系统中不存在的表批量插入到数据库中
        insertMonitorTables();

        // 耗时较多，因为是直接批量插入到数据库中；2022-10-21 09:47:32
        // 将带有用户名的链路信息持久化到MySQL数据库中；
        flushSegmentDetailToDb(segmentDetailDoList);

        // 耗时较多，因为是直接批量插入到数据库中；2022-10-21 09:47:32
        // 将没有用户名的链路信息持久化到MySQL数据库中；
        flushSegmentDetailUserNameIsNullToDb(segmentDetailUserNameIsNullDoList);
    }
}
