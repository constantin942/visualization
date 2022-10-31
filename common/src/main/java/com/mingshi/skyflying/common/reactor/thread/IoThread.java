package com.mingshi.skyflying.common.reactor.thread;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.agent.AgentInformationSingleton;
import com.mingshi.skyflying.common.config.GracefulShutdown;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.reactor.queue.InitProcessorByLinkedBlockingQueue;
import com.mingshi.skyflying.common.reactor.queue.IoThreadLinkedBlockingQueue;
import com.mingshi.skyflying.common.statistics.InformationOverviewSingleton;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * @Author zhaoming
 * @Description 每一个线程从队列中获取匹配后的数据，然后在本地统计；当超过了一定时间后，再统一发送到Redis中；
 * @Date 18:15 2021/10/13
 * @Param
 * @return
 **/
@Slf4j
public class IoThread extends Thread {

    /**
     * 所有的IoThread线程共享同一个公共有界阻塞队列；2022-06-01 10:22:49
     */
    private LinkedBlockingQueue<ObjectNode> ioThreadLinkedBlockingQueue;
    private Instant currentTime = null;

    private Integer flushToRocketMqInterval = Const.FLUSH_TO_MQ_INTERVAL;
    private Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentHeartBeatMap = null;
    private Map<String/* 时间 */, Integer/* 消费的数量 */> processorThreadQpsMap = null;
    private LinkedList<MsSegmentDetailDo> segmentDetailDoList = null;
    private LinkedList<MsSegmentDetailDo> segmentDetailUserNameIsNullDoList = null;
    private HashSet<String> userHashSet = null;
    private MingshiServerUtil mingshiServerUtil;
    private Integer capacity;

    private Map<String, Integer> everydayVisitedTimesMap = null;
    /**
     * 用户总的访问次数；2022-10-14 13:59:31
      */
    private Map<String, Integer> userAccessBehaviorAllVisitedTimesMap = null;
    /**
     * 用户最后的访问时间；2022-10-14 14:05:16
      */
    private  Map<String, String> userAccessBehaviorLatestVisitedTimeMap = null;
    /**
     * 表最后的访问时间；2022-10-14 14:05:16
     */
    private Map<String, String> tableLatestVisitedTimeMap = null;
    /**
     * 统计表每天的访问次数；2022-10-14 14:22:59
     */
    private Map<String/* 表名 */, Map<String/* 每天的日期，格式是：yyyy-MM-dd */, Long/* 访问次数 */>> tableEverydayVisitedTimesMap = null;
    /**
     * 统计用户访问过的表的次数；2022-10-14 14:55:50
     */
    private Map<String/* 用户名 */, Map<String/* 表名 */, Double/* 访问次数 */>> userAccessBehaviorAllVisitedTablesMap = null;
    /**
     * 统计表被用户访问过的次数；2022-10-14 15:11:17
     */
    private Map<String/* 表名 */, Map<String/* 用户名 */, Double/* 访问次数 */>> tableByHowManyUserVisitedMap = null;
    /**
     * 统计每个表操作类型次数；2022-10-14 16:48:04
     */
    private Map<String/* 表名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> tableOperationTypeMap = null;
    /**
     * 统计每个用户的操作类型次数
     */
    private Map<String/* 用户名 */, Map<String/* 操作类型 */, Double/* 操作次数 */>> userOperationTypeMap = null;

    public IoThread(Integer queueSize, MingshiServerUtil mingshiServerUtil) {
        currentTime = Instant.now().minusSeconds(new Random().nextInt(Const.CURRENT_TIME_RANDOM));
        // 懒汉模式：只有用到的时候，才创建list实例。2022-06-01 10:22:16
        skywalkingAgentHeartBeatMap = new HashMap<>(Const.NUMBER_EIGHT);
        processorThreadQpsMap = new HashMap<>(Const.NUMBER_EIGHT);
        userHashSet = new HashSet<>();
        segmentDetailDoList = new LinkedList<>();
        segmentDetailUserNameIsNullDoList = new LinkedList<>();
        this.ioThreadLinkedBlockingQueue = new LinkedBlockingQueue<>(queueSize);
        this.mingshiServerUtil = mingshiServerUtil;
        this.capacity = queueSize;

        this.everydayVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.userAccessBehaviorAllVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.userAccessBehaviorLatestVisitedTimeMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.tableLatestVisitedTimeMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.tableEverydayVisitedTimesMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.userAccessBehaviorAllVisitedTablesMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.tableByHowManyUserVisitedMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.tableOperationTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
        this.userOperationTypeMap = new HashMap<>(Const.NUMBER_EIGHT);
    }

    /**
     * <B>方法名称：offer</B>
     * <B>概要说明：往IoThread线程内部的线程中存放清洗后的消息</B>
     *
     * @return java.lang.Boolean
     * @Author zm
     * @Date 2022年09月27日 14:09:32
     * @Param [objectNode]
     **/
    public Boolean offer(ObjectNode objectNode) {
        return ioThreadLinkedBlockingQueue.offer(objectNode);
    }

    /**
     * <B>方法名称：getQueueSize</B>
     * <B>概要说明：获取队列中元素的个数</B>
     *
     * @return java.lang.Integer
     * @Author zm
     * @Date 2022年09月27日 14:09:34
     * @Param []
     **/
    public Integer getQueueSize() {
        return ioThreadLinkedBlockingQueue.size();
    }

    /**
     * <B>方法名称：getQueueCapacity</B>
     * <B>概要说明：获取队列容量</B>
     *
     * @return java.lang.Integer
     * @Author zm
     * @Date 2022年09月27日 14:09:09
     * @Param []
     **/
    public Integer getQueueCapacity() {
        return capacity;
    }

    @Override
    public void run() {
        int queueSize = -1;
        try {
            while (Boolean.TRUE.equals(GracefulShutdown.getRunning())) {
                doRun(Boolean.FALSE);
            }
            queueSize = ioThreadLinkedBlockingQueue.size();
            log.error("# IoThread.run() # IoThread线程 = 【{}】要退出了。此时jvm关闭的标志位 = 【{}】，还没有执行finally代码块之前，线程对应的队列中元素的个数 = 【{}】。",
                Thread.currentThread().getName(),
                Boolean.TRUE.equals(GracefulShutdown.getRunning()),
                queueSize);
        } finally {
            Instant now = Instant.now();
            try {
                while (!ioThreadLinkedBlockingQueue.isEmpty() || Const.NUMBER_ZERO < InitProcessorByLinkedBlockingQueue.getProcessorGraceShutdown()) {
                    doRun(Boolean.FALSE);
                }
                log.error("# IoThread.run() # IoThread线程 = 【{}】要退出了。该线程对应的队列中元素的个数 = 【{}】。处理完【{}】条消息用时【{}】毫秒。当前存活的processor线程数量 = 【{}】。",
                    Thread.currentThread().getName(),
                    ioThreadLinkedBlockingQueue.size(),
                    queueSize,
                    DateTimeUtil.getTimeMillis(now),
                    InitProcessorByLinkedBlockingQueue.getProcessorGraceShutdown()
                );
            } finally {
                // 当IoThread线程退出时，要把本地攒批的数据保存到MySQL数据库中；2022-06-01 10:32:43
                insertSegmentDetailIntoMySqlAndRedis();
                // 当前线程退出时，减一。减一后的结果，aiitKafkaConsumer在优雅关机时会用到。2022-10-08 15:56:54
                IoThreadLinkedBlockingQueue.decrementIoThreadGraceShutdown();
            }

        }
    }

    private void doRun(Boolean isRunning) {
        try {
            ObjectNode jsonObject = ioThreadLinkedBlockingQueue.poll();
            if (null == jsonObject) {
                if (Boolean.FALSE.equals(isRunning)) {
                    TimeUnit.MILLISECONDS.sleep(Const.NUMBER_TEN);
                }
            } else {
                // 从json实例中获取探针名称信息，用于心跳；2022-06-27 13:40:44
                getSkywalkingAgentNameFromJsonObject(jsonObject);

                // 统计processorThread线程的QPS；2022-07-23 11:15:29
                getProcessorThreadQpsFromJsonObject(jsonObject);

                // 从json实例中获取segmentDetail实例的信息
                getSegmentDetailFromJsonObject(jsonObject);

                // 从json实例中获取用户名为空的segmentDetail实例信息
                getSegmentDetailUserNameIsNullFromJsonObject(jsonObject);

            }

            // 将segment信息和SQL审计日志插入到表中；2022-05-30 17:50:12
            insertSegmentDetailIntoMySqlAndRedis();
        } catch (Throwable e) {
            log.error("# IoThread.run() # 将segment信息、及对应的索引信息和SQL审计日志信息在本地攒批和批量插入时 ，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getSkywalkingAgentNameFromJSONObject</B>
     * <B>概要说明：获取探针名称，用于更新探针心跳</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年06月27日 13:06:49
     * @Param [jsonObject]
     **/
    private void getSkywalkingAgentNameFromJsonObject(ObjectNode jsonObject) {
        try {
            String listString = null;
            JsonNode jsonNode = jsonObject.get(Const.SKYWALKING_AGENT_HEART_BEAT_DO_LIST);
            if (null != jsonNode) {
                listString = jsonNode.asText();
            }
            if (StringUtil.isNotBlank(listString)) {
                Map<String/* skywalking探针名字 */, String/* skywalking探针最近一次发来消息的时间 */> skywalkingAgentTimeMap = JsonUtil.string2Obj(listString, Map.class);
                Set<String> stringSet = skywalkingAgentTimeMap.keySet();
                for (String set : stringSet) {
                    Map<String, String> map = JsonUtil.string2Obj(set, Map.class);
                    String serviceCode = map.get(Const.SERVICE_CODE);
                    AgentInformationSingleton.put(serviceCode, Const.DOLLAR);
                }
                skywalkingAgentHeartBeatMap.putAll(skywalkingAgentTimeMap);
            }
        } catch (Exception e) {
            log.error("# IoThread.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getProcessorThreadQpsFromJSONObject</B>
     * <B>概要说明：获取QPS数据</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月23日 11:07:40
     * @Param [jsonObject]
     **/
    private void getProcessorThreadQpsFromJsonObject(ObjectNode jsonObject) {
        try {
            String listString = null;
            JsonNode jsonNode = jsonObject.get(Const.QPS_ZSET_EVERY_PROCESSOR_THREAD);
            if (null != jsonNode) {
                listString = jsonNode.asText();
            }
            if (StringUtil.isNotBlank(listString)) {
                Map<String, Integer> timeCountMap = JsonUtil.string2Obj(listString, HashMap.class);
                if (0 == processorThreadQpsMap.size()) {
                    if (null != timeCountMap && 0 < timeCountMap.size()) {
                        processorThreadQpsMap.putAll(timeCountMap);
                    }
                } else {
                    Iterator<String> iterator = timeCountMap.keySet().iterator();
                    while (iterator.hasNext()) {
                        String time = iterator.next();
                        Integer count = timeCountMap.get(time);
                        Integer accumuCount = processorThreadQpsMap.get(time);
                        if (null == accumuCount && null != count) {
                            processorThreadQpsMap.put(time, count);
                        } else if (null != accumuCount && null != count) {
                            processorThreadQpsMap.put(time, count + accumuCount);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("# IoThread.getSkywalkingAgentNameFromJSONObject() # 将skywalking探针名称信息放入到 skywalkingAgentHeartBeatList 中出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getSegmentDetailFromJsonObject</B>
     * <B>概要说明：获取用户访问详情信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-15 09:34:56
     * @Param [jsonObject]
     **/
    private void getSegmentDetailFromJsonObject(ObjectNode jsonObject) {
        try {
            String listString = doSegmentDetaiDolList(jsonObject);
            if (StringUtil.isNotBlank(listString)) {
                LinkedList<MsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, MsSegmentDetailDo.class);
                // 之所将用户名判断放到这里来判断，而不是放到processor线程中判断，原因是为了提高效率。
                // 如果要放到processor中进行判断，如果该用户不存在，那么需要将其保存到Redis中。
                // 在保存到Redis中的时候，需要将其加锁。一旦在processor中加锁，就降低了processor线程的性能。
                for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailList) {
                    String userName = msSegmentDetailDo.getUserName();
                    String tableName = msSegmentDetailDo.getMsTableName();
                    if (StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(tableName)) {
                        // 判断用户是否已存在，如果不存在，那么先暂存起来，然后再将其发送到Redis中；2022-07-19 10:03:22
                        Boolean userIsExisted = InformationOverviewSingleton.userIsExisted(userName);
                        if (false == userIsExisted) {
                            userHashSet.add(userName);
                        }
                    }

                    // 对各个指标进行统计；2022-10-14 17:29:03
                    mingshiServerUtil.doStatistics(msSegmentDetailDo, everydayVisitedTimesMap, userAccessBehaviorAllVisitedTimesMap, userAccessBehaviorLatestVisitedTimeMap, tableLatestVisitedTimeMap, tableEverydayVisitedTimesMap, userAccessBehaviorAllVisitedTablesMap, tableByHowManyUserVisitedMap, tableOperationTypeMap, userOperationTypeMap);

                }
                segmentDetailDoList.addAll(segmentDetailList);
            }
        } catch (Exception e) {
            log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
        }
    }

    private String doSegmentDetaiDolList(ObjectNode jsonObject) {
        String listString = null;
        try {
            JsonNode jsonNode = jsonObject.get(Const.SEGMENT_DETAIL_DO_LIST);
            if (null != jsonNode) {
                listString = jsonNode.asText();
            }
        } catch (Exception e) {
            log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
        }
        return listString;
    }

    private void getSegmentDetailUserNameIsNullFromJsonObject(ObjectNode jsonObject) {
        try {
            String listString = doSegmentDetaiUserNameIsNullDolList(jsonObject);
            if (StringUtil.isNotBlank(listString)) {
                LinkedList<MsSegmentDetailDo> segmentDetailList = JsonUtil.string2Obj(listString, LinkedList.class, MsSegmentDetailDo.class);
                segmentDetailUserNameIsNullDoList.addAll(segmentDetailList);
            }
        } catch (Exception e) {
            log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
        }
    }

    private String doSegmentDetaiUserNameIsNullDolList(ObjectNode jsonObject) {
        String listString = null;
        try {
            JsonNode jsonNode = jsonObject.get(Const.SEGMENT_DETAIL_USERNAME_IS_NULL_DO_LIST);
            if (null != jsonNode) {
                listString = jsonNode.asText();
            }
        } catch (Exception e) {
            log.error("# IoThread.getSegmentDetailFromJSONObject() # 将segmentDetail实例信息放入到 segmentDetailList 中出现了异常。", e);
        }
        return listString;
    }

    /**
     * <B>方法名称：insertSegmentDetailIntoMySQLAndRedis</B>
     * <B>概要说明：将来自skywalking的segmentDetail信息保存到MySQL数据库和Redis分布式缓存中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年05月30日 17:05:27
     * @Param [jsonObject]
     **/
    private void insertSegmentDetailIntoMySqlAndRedis() {
        try {
            long isShouldFlush = DateTimeUtil.getSecond(currentTime) - flushToRocketMqInterval;
            if (isShouldFlush >= 0 || Boolean.FALSE.equals(GracefulShutdown.getRunning())) {
                // 当满足了间隔时间或者jvm进程退出时，就要把本地攒批的数据保存到MySQL数据库中；2022-06-01 10:38:04
                mingshiServerUtil.doInsertSegmentDetailIntoMySqlAndRedis(userHashSet, processorThreadQpsMap, skywalkingAgentHeartBeatMap, segmentDetailDoList, segmentDetailUserNameIsNullDoList);
                currentTime = Instant.now();

                // 将数据统计信息发送到Redis中；2022-10-15 09:37:43
                mingshiServerUtil.flushStatisticsToRedis(everydayVisitedTimesMap, userAccessBehaviorAllVisitedTimesMap, userAccessBehaviorLatestVisitedTimeMap, tableLatestVisitedTimeMap, tableEverydayVisitedTimesMap, userAccessBehaviorAllVisitedTablesMap, tableByHowManyUserVisitedMap, tableOperationTypeMap, userOperationTypeMap);
            }
        } catch (Exception e) {
            log.error("# IoThread.insertSegmentAndIndexAndAuditLog() # 将来自skywalking的segment信息和SQL审计信息插入到表中出现了异常。", e);
        }
    }
}
