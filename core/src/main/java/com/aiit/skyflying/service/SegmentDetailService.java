package com.aiit.skyflying.service;

import com.aiit.skyflying.common.domain.*;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;

import java.text.ParseException;
import java.util.List;
import java.util.Map;

public interface SegmentDetailService extends ParentService<SegmentDetailDo, Long> {

  ServerResponse<String> getAllSegmentsBySegmentRelation(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, String operationType, Integer pageNo, Integer pageSize);

  ServerResponse<String> getAllUserName();

  ServerResponse<String> getAllMsTableName();

  ServerResponse<InstanceTable> getAllInstanceAndTableName();

  ServerResponse<List<String>> getCountsOfUser(String tableName);

  ServerResponse<List<String>> getUserOperationTypeCount(String userName);

  ServerResponse<String> getCoarseCountsOfUser();

  ServerResponse<List<Long>> getCountsOfUserUserRecentSevenDays(String msTableName, String startTime, String endTime, Integer pageNo, Integer pageSize) throws ParseException;

  ServerResponse<List<Long>> getCountsOfEveryonRecentSevenDays(String userName, String startTime, String endTime);

  ServerResponse<Map<String, List<UserUsualAndUnusualVisitedData>>> getUserUsualAndUnusualData(String applicationUserName);

  ServerResponse<List<Long>> getCountsOfAllRecentSevenDays(String startTime, String endTime);

  ServerResponse<SystemOverview> getOverviewOfSystem();

  ServerResponse<String> getCoarseCountsOfTableName(String dbName, String tableName, Integer pageNo, Integer pageSize);

  ServerResponse<List<AlarmData>> getAlarmData();

  ServerResponse<List<UserAlarmData>> getUserAlarmData();

  ServerResponse<String> getCoarseCountsOfUsers(String username, Integer pageNo, Integer pageSize);
}

// 恢复kafak的某个topic下某个消费组的offset为最早；2022-06-07 10:25:58
// bin/kafka-consumer-groups.sh --bootstrap-server localhost:9092 --group skyflying-consumer-group --reset-offsets --all-topics --to-earliest --execute
// bin/kafka-consumer-groups.sh --bootstrap-server localhost:9092 --group skyflying-consumer-group --reset-offsets --topic skywalking-segments --to-datetime 2022-04-13T00:00:00.000 --execute


// 根据时间，恢复恢复kafak的某个topic下某个消费组的offset；2022-06-30 16:49:40
// bin/kafka-consumer-groups.sh --bootstrap-server 10.0.107.49:9092 --group skyflying-consumer-group-online --reset-offsets --topic skywalking-segments-online --to-datetime 2022-10-10T08:00:00.000 --execute
// bin/kafka-consumer-groups.sh --bootstrap-server 10.0.107.49:9092 --group skyflying-consumer-group-online --reset-offsets --topic skywalking-segments-online --to-datetime 2022-10-24T11:00:00.000 --execute
// bin/kafka-consumer-groups.sh --bootstrap-server 10.0.107.49:9092 --group skyflying-consumer-group-online --reset-offsets --topic skywalking-segments-online --to-datetime 2022-11-09T11:00:00.000 --execute
// bin/kafka-consumer-groups.sh --bootstrap-server 10.0.107.49:9092 --group test-skyflying-consumer-group --reset-offsets --topic test-skywalking-segments --to-datetime 2022-07-29T08:00:00.000 --execute

// topic的数据改为保存60天
// bin/kafka-configs.sh --bootstrap-server localhost:9092  --entity-type topics --entity-name skywalking-segments --alter --add-config retention.ms=5184000000


// 查看更改后的效果
// bin/kafka-topics.sh --bootstrap-server localhost:9092  --topic skywalking-segments --describe
