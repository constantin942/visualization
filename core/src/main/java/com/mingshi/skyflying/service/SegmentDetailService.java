package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.InstanceTable;
import com.mingshi.skyflying.domain.SegmentDetailDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface SegmentDetailService extends ParentService<SegmentDetailDo, Long> {

  ServerResponse<String> getAllSegmentsBySegmentRelation(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserName();

  ServerResponse<String> getAllMsTableName();

  ServerResponse<InstanceTable> getAllInstanceAndTableName();
}
// 恢复kafak的某个topic下某个消费组的offset为最早；2022-06-07 10:25:58
// bin/kafka-consumer-groups.sh --bootstrap-server localhost:9092 --group skyflying-consumer-group --reset-offsets --all-topics --to-earliest --execute


// 根据时间，恢复恢复kafak的某个topic下某个消费组的offset；2022-06-30 16:49:40
// bin/kafka-consumer-groups.sh --bootstrap-server 10.0.107.49:9092 --group skyflying-consumer-group --reset-offsets --topic skywalking-segments --to-datetime 2022-05-31T08:00:00.000 --execute

// topic的数据改为保存60天
// bin/kafka-configs.sh --bootstrap-server localhost:9092  --entity-type topics --entity-name skywalking-segments --alter --add-config retention.ms=5184000000


// 查看更改后的效果
// bin/kafka-topics.sh --bootstrap-server localhost:9092  --topic skywalking-segments --describe
