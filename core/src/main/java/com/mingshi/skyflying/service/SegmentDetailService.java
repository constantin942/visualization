package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.SegmentDetailDo;
import com.mingshi.skyflying.response.ServerResponse;

public interface SegmentDetailService extends ParentService<SegmentDetailDo, Long> {

  ServerResponse<String> getAllSegmentsBySegmentRelation(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserName();

  ServerResponse<String> getAllMsTableName();

}
// 恢复kafak的某个topic下某个消费组的offset为最早；2022-06-07 10:25:58
// bin/kafka-consumer-groups.sh --bootstrap-server localhost:9092 --group skyflying-consumer-group --reset-offsets --all-topics --to-earliest --execute
// topic的数据改为保存60天
// bin/kafka-configs.sh --bootstrap-server localhost:9092  --entity-type topics --entity-name skywalking-segments --alter --add-config retention.ms=518400000
// 查看更改后的效果
// bin/kafka-topics.sh --bootstrap-server localhost:9092  --topic skywalking-segments --describe
