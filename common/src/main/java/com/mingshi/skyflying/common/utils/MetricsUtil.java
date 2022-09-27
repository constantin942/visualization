package com.mingshi.skyflying.common.utils;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * <B>主类名称: MetricsUtil</B>
 * <B>概要说明：统计第一层内存队列、第二层内存队列、第三层内存队列使用情况，从而给出优化建议。因为这三层的队列使用能够反映出这三层线程的繁忙程度。</B>
 * Author zm
 * Date 2022/9/26 21:15
 *
 * @Version 1.0
 **/
public class MetricsUtil {
    private static final Map<String/* 第二层哪一个队列 */, Map<String/* 分区编号 */, AtomicInteger/* 目前已累计的消息数量 */>> queuePartitionMap = new ConcurrentHashMap<>();

    /**
     * <B>方法名称：put</B>
     * <B>概要说明：将数据放入到队列里进行统计</B>
     * @Author zm
     * @Date 2022年09月26日 21:09:26
     * @Param [queueId, partitionId]
     * @return void
     **/
    public static void put(String queueId, String partitionId) {
        Map<String, AtomicInteger> partitionMap = queuePartitionMap.get(queueId);
        if (null == partitionMap) {
            partitionMap = new ConcurrentHashMap<>();
            queuePartitionMap.put(queueId, partitionMap);
        }
        AtomicInteger count = partitionMap.get(partitionId);
        if (null == count) {
            partitionMap.put(partitionId, new AtomicInteger(1));
        } else {
            count.incrementAndGet();
            partitionMap.put(partitionId, count);
        }
    }

    /**
     * <B>方法名称：getQueuePartitionMap</B>
     * <B>概要说明：获取统计队列</B>
     * @Author zm
     * @Date 2022年09月26日 21:09:10
     * @Param []
     * @return java.util.Map<java.lang.String,java.util.Map<java.lang.String,java.util.concurrent.atomic.AtomicInteger>>
     **/
    public static Map<String/* 第二层哪一个队列 */, Map<String/* 分区编号 */, AtomicInteger/* 目前已累计的消息数量 */>> getQueuePartitionMap(){
        return queuePartitionMap;
    }
}
