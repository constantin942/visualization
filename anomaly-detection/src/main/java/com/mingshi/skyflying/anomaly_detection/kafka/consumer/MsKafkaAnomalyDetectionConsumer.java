package com.mingshi.skyflying.anomaly_detection.kafka.consumer;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.config.SpringUtil;
import com.mingshi.skyflying.common.config.GracefulShutdown;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.enums.RecordEnum;
import com.mingshi.skyflying.common.kafka.producer.records.MsConsumerRecords;
import com.mingshi.skyflying.common.reactor.queue.IoThreadLinkedBlockingQueue;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.KafkaConsumer;
import org.apache.kafka.common.serialization.BytesDeserializer;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.utils.Bytes;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.springframework.context.ApplicationContext;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <B>方法名称：MsKafkaAnomalyDetectionConsumer</B>
 * <B>概要说明：异常检测用的消费者</B>
 *
 * @Author zm
 * @Date 2022-10-17 13:52:40
 * @Param
 * @return
 **/
@Slf4j
public class MsKafkaAnomalyDetectionConsumer extends Thread {
    private List<String> consumerTopic;
    private String consumerGroup;
    private MingshiServerUtil mingshiServerUtil;
    private String bootstrapServers;
    private AnomalyDetectionBusiness anomalyDetectionBusiness;
    /**
     * 初始化完成的标志；2022-07-28 17:12:32
     */
    private volatile AtomicBoolean isInitDone = new AtomicBoolean(false);

    private KafkaConsumer<String, Bytes> aiitKafkaConsumer = null;

    public MsKafkaAnomalyDetectionConsumer(AnomalyDetectionBusiness anomalyDetectionBusiness, String bootstrapServers, List<String> consumerTopic, String consumerGroup, MingshiServerUtil mingshiServerUtil) {
        this.bootstrapServers = bootstrapServers;
        this.consumerTopic = consumerTopic;
        this.consumerGroup = consumerGroup;
        this.anomalyDetectionBusiness = anomalyDetectionBusiness;
        this.mingshiServerUtil = mingshiServerUtil;
    }

    /**
     * <B>方法名称：init</B>
     * <B>概要说明：初始化消费者配置信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月28日 17:07:39
     * @Param []
     **/
    public void init() {
        Properties properties = new Properties();
        properties.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, bootstrapServers);
        // 消费分组名
        properties.put(ConsumerConfig.GROUP_ID_CONFIG, consumerGroup);
        // 是否自动提交offset
        properties.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false");
        // 当消费主题的是一个新的消费组，或者指定offset的消费方式，offset不存在，那么应该如何消费
        // latest(默认) ：只消费自己启动之后才发送到主题的消息
        // earliest：第一次从头开始消费，以后按照消费offset记录继续消费，这个需要区别于consumer.seekToBeginning(每次都从头开始消费)
        properties.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");
        // consumer给broker发送心跳的间隔时间，broker接收到心跳如果此时有rebalance发生会通过心跳响应将rebalance方案下发给consumer，这个时间可以稍微短一点
        properties.put(ConsumerConfig.HEARTBEAT_INTERVAL_MS_CONFIG, 1000);
        // broker接收不到一个consumer的心跳, 持续该时间, 就认为故障了，会将其踢出消费组，对应的Partition也会被重新分配给其他consumer，默认是10秒
        properties.put(ConsumerConfig.SESSION_TIMEOUT_MS_CONFIG, 30 * 1000);
        // 一次poll最大拉取消息的条数，如果消费者处理速度很快，可以设置大点，如果处理速度一般，可以设置小点
        properties.put(ConsumerConfig.MAX_POLL_RECORDS_CONFIG, 100);
        // 如果两次poll操作间隔超过了这个时间，broker就会认为这个consumer处理能力太弱，会将其踢出消费组，将分区分配给别的consumer消费
        properties.put(ConsumerConfig.MAX_POLL_INTERVAL_MS_CONFIG, 60 * 1000);
        // 把消息的key从字节数组反序列化为字符串
        properties.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class.getName());
        // 把消息的value从字节数组反序列化为字符串
        properties.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, BytesDeserializer.class.getName());

        aiitKafkaConsumer = new KafkaConsumer<>(properties);

        // 订阅主题
        aiitKafkaConsumer.subscribe(consumerTopic);
        isInitDone.set(true);
    }

    @Override
    public void run() {
        init();
        if (false == isInitDone.get()) {
            log.error("# MsKafkaAnomalyDetectionConsumer.run() # 初始化失kafka消费者败，不能消费kafka服务端的消息。");
            return;
        }
        doRun();
    }

    private void doRun() {
        try {
            while (Boolean.TRUE.equals(GracefulShutdown.getRUNNING())) {
                if (Boolean.FALSE.equals(MsCaffeineCache.getUserPortraitInitDone())) {
                    try {
                        TimeUnit.MILLISECONDS.sleep(1000);
                        log.error("# MsKafkaAnomalyDetectionConsumer.doRun() # 开始执行异常检测，由于用户画像还没有初始化完毕，在这里循环等待。");
                    } catch (Exception e) {
                        // ignore
                    }
                } else {
                    // 用户画像已初始化完毕，消费消息；2022-10-17 15:25:14
                    consumeRecords();
                }
            }
        } finally {
            try {
                log.info("# MsKafkaAnomalyDetectionConsumer.doRun() # 优雅关机，开始同步提交offset。");
                doCommitSync();
                log.info("# MsKafkaAnomalyDetectionConsumer.doRun() # 优雅关机，同步提交offset完毕。");
            } finally {
                aiitKafkaConsumer.close();
            }
        }
    }

    /**
     * <B>方法名称：consumeRecords</B>
     * <B>概要说明：从Kafka服务端拉取消息并消费消息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 16:10:38
     * @Param []
     **/
    private void consumeRecords() {
        try {
            ConsumerRecords<String, Bytes> records = aiitKafkaConsumer.poll(Duration.ofMillis(200));
            Integer count = records.count();
            for (ConsumerRecord<String, Bytes> consumerRecord : records) {
                byte[] bytes = consumerRecord.value().get();
                String recordStr = new String(bytes);
                MsConsumerRecords msConsumerRecords = JsonUtil.string2Obj(recordStr, MsConsumerRecords.class);
                if (msConsumerRecords.getRecordType().equals(RecordEnum.MSSEGMENTDETAILDO_CONSUME_FAILED.getCode())) {
                    // 对待异常检测的数据（之前是因为用户画像没有初始化完毕），再次进行异常检测；2022-10-19 11:23:24
                    msSegmentDetailDoReConsume(msConsumerRecords);
                } else if (msConsumerRecords.getRecordType().equals(RecordEnum.ANOMALY_ALARM.getCode())) {
                    List<MsAlarmInformationDo> alarmInformationDos = (List<MsAlarmInformationDo>) msConsumerRecords.getBody();
                    // 将异常信息保存到数据库中；2022-10-19 10:55:04
                    mingshiServerUtil.flushAbnormalToDb(alarmInformationDos);

                    // 对异常信息进行钉钉告警；2022-10-19 10:55:34
                    ObjectMapper mapper = new ObjectMapper();
                    alarmInformationDos = mapper.convertValue(alarmInformationDos, new TypeReference<List<MsAlarmInformationDo>>() {});
                    // 由于该类没有交由Spring管理, 所以这里采用反射的方式调用Spring管理的类
                    ApplicationContext applicationContext = SpringUtil.getApplicationContext();
                    AnomalyDetectionBusiness anomalyDetectionBusiness = applicationContext.getBean(AnomalyDetectionBusiness.class);
                    anomalyDetectionBusiness.dingAlarm(alarmInformationDos);
                }
            }
            if (count > 0) {
                // 手动异步提交offset，当前线程提交offset不会阻塞，可以继续处理后面的程序逻辑
                aiitKafkaConsumer.commitAsync();
            }
        } catch (Exception e) {
            log.error(" # MsKafkaAnomalyDetectionConsumer.consumeRecords() # 消费消息时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：msSegmentDetailDoReConsume</B>
     * <B>概要说明：进行异常检测</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-10-17 15:58:00
     * @Param [msConsumerRecords]
     **/
    private void msSegmentDetailDoReConsume(MsConsumerRecords msConsumerRecords) {
        Instant now = Instant.now();
        // 进行异常检测；2022-10-17 13:59:46
        List<MsSegmentDetailDo> segmentDetaiDolList = (List<MsSegmentDetailDo>) msConsumerRecords.getBody();
        // 进行异常检测
        anomalyDetectionBusiness.doUserVisitedIsAbnormal(segmentDetaiDolList);
        long timeMillis = DateTimeUtil.getTimeMillis(now);
        if(Const.NUM_FIVE < timeMillis){
            log.info("# MsKafkaAnomalyDetectionConsumer.msSegmentDetailDoReConsume() # 异常检测【{}条】耗时【{}】毫秒。", segmentDetaiDolList.size(), DateTimeUtil.getTimeMillis(now));
        }
    }

    /**
     * <B>方法名称：doCommitSync</B>
     * <B>概要说明：等待所有的ioThread线程都退出后，再提交offset</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 16:10:41
     * @Param []
     **/
    private void doCommitSync() {
        try {
            while (0 < IoThreadLinkedBlockingQueue.getIoThreadGraceShutdown()) {
                TimeUnit.MILLISECONDS.sleep(Const.NUM_ONE);
            }
            Instant now = Instant.now();
            log.error("# MsKafkaSegmentsConsumer.doCommitSync() # 优雅关机，所有的IoThread线程都已退出，此时再同步提交offset。");
            aiitKafkaConsumer.commitSync();
            log.error("# MsKafkaSegmentsConsumer.doCommitSync() # 优雅关机，所有的IoThread线程都已退出，此时再同步提交offset，用时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
        } catch (InterruptedException e) {
            log.error("# MsKafkaSegmentsConsumer.doCommitSync() # 优雅关机，同步提交offset时，出现了异常。", e);
        }
    }
}
