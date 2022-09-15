/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.mingshi.skyflying.common.constant;

public class Const {

  // 数据库字段长度限制
  public static final Integer DATA_LENTGH = 128;
  public static final String METHOD = "method";
  public static final String PARAMS = "params";
  public static final String ORDER_ID = "orderId";
  public static final String UPDATE_MONITOR_TABLE = "updateMonitorTable";
  public static final String UPDATE_MONITOR_TABLE_DESC = "updateMonitorTableaDesc";
  public static final String UPDATE_SKYWALKING_AGENT = "updateSkywalkingAgent";
  public static final String IS_DELETE_OPRETION = "isDelete";
  public static final String TABLE_DESC = "tableDesc";
  public static final String AGENT_NAME = "agentName";
  public static final String AGENT_CODE = "agentCode";
  public static final String TOPIC = "topic";
  public static final String PARTITION = "partition";
  public static final String OFFSET = "offset";
  public static final String TOPIC_PARTITION_OFFSET = "topic-partition-offset";
  public static final String HASH_TEST_GRACEFUL_SHUTDOWN = "hash-test-graceful-shutdown";

  public interface RedisCacheExtime {
    int REDIS_SESSION_EXTIME = 60 * 60 * 3;//3小时
  }

  public static final String SCHEDULED_GET_SEGMENT_DETAIL_DO = "redisson:key:scheduledGetSegmentDetailDo";
  public static final String SCHEDULED_UPDATE_USER_NAME_BY_TOKEN = "redisson:key:scheduledUpdateUserNameByToken";
  public static final String SCHEDULED_UPDATE_USER_NAME_BY_GLOBAL_TRACEID = "redisson:key:scheduledUpdateUserNameByGlobalTraceId";
  public static final String SCHEDULED_UPDATE_USER_PORTRAIT = "redisson:key:scheduledUpdateUserPortrait";
  public static final String SCHEDULED_GET_NO_CHECK_ABNORMAL_RECORD = "redisson:key:scheduledGetNoCheckAbnormalRecord";
  public static final String SCHEDULED_GET_DMS_AUDIT_LOG = "redisson:key:scheduledGetDmsAuditLog";

  public static final String AGENT_STATUS_UNKNOWN = "状态未知";
  public static final String AGENT_ON_OFF = "agent_on_off";
  public static final String AGENT_QUERY = "agent_query";
  public static final String AGENT_STATUS_ON = "on";
  public static final String AGENT_STATUS_OFF = "off";

  public static final String MSG = "msg";
  public static final String RESPONSE_STATUS = "responseStatus";
  public static final String AGENT_STATUS = "agentStatus";
  public static final String SERVICE_INSTANCE = "serviceInstance";
  public static final String REQUEST_ID = "requestId";
  public static final String AGENT_OPERATION_TYPE = "operationType";

  public static final Integer NUMBER_EIGHT = 8;
  public static final String MYSQL = "mysql";
  public static final String FAIL = "FAIL";
  public static final String PAGE_NO = "pageNo";
  public static final String PAGE_SIZE = "pageSize";
  public static final String SUCCESS = "success";
  public static final String SUCCESS_MSG = "获取数据成功！";
  public static final String FAILED = "failed";
  public static final String SPANID = "spanId";
  public static final String PARENT_SPAN_ID = "parentSpanId";
  public static final String ENDPOINT_NAME = "endpointName";
  public static final String COMPONET = "component";
  public static final String LOGS = "logs";
  public static final String JEDIS_SENTINEL_GET_MASTER_ADDR_BY_NAME = "Jedis/sentinelGetMasterAddrByName";
  public static final String REDIS = "Redis";
  public static final String HTTP_BODY = "http.body";
  public static final String URL = "url";
  public static final CharSequence DING_TALK = "dingtalk";
  public static final String HTTP_METHOD = "http.method";
  public static final String TAGS = "tags";
  public static final Object USERNAME = "username";
  public static final Object NICKNAME = "nickname";
  public static final Integer NUMBER_ONE = 1;
  public static final String REDISSON_PING = "Redisson/PING";
  public static final String LETTUCE_SENTINEL = "Lettuce/SENTINEL";
  public static final String MYSQL_JDBI_CONNECTION_CLOSE = "Mysql/JDBI/Connection/close";
  public static final String GET_DEVICES_NOTIFICATION = "GET:/devices/notification";
  public static final String GET_MANAGER_HTML = "GET:/manager/html";
  public static final String BALANCER_USER_CHECKTOKEN = "Balancer/user/checkToken";
  public static final String GET1 = "GET:";
  public static final String GET_ASSETS_FONTS_NUNITO_BOLD_WOFF2 = "GET:/assets/fonts/Nunito-Bold.woff2";
  public static final String GET_TEMP_NULL = "GET:/temp/null";
  public static final String GET_ASSETS_FONTS_NIOICON_TTF = "GET:/assets/fonts/Nioicon.ttf";
  public static final String GET_ASSETS_FONTS_NUNITO_REGULAR_WOFF2 = "GET:/assets/fonts/Nunito-Regular.woff2";
  public static final String GET_ASSETS_FONTS_ROBOTO_REGULAR_WOFF2 = "GET:/assets/fonts/Roboto-Regular.woff2";
  public static final String GET_ASSETS_FONTS_ROBOTO_MEDIUM_WOFF2 = "GET:/assets/fonts/Roboto-Medium.woff2";
  public static final String HIKARICP_CONNECTION_GETCONNECTION = "HikariCP/Connection/getConnection";
  public static final String GET2 = "GET:/";
  public static final String GET_COMPANIES_COMPANYHEALTH_LIST = "GET:/companies/companyHealth/list";
  public static final String NULL_NULL = "null:null";
  public static final String MYSQL_JDBI_PREPARED_STATEMENT_EXECUTE_UPDATE = "Mysql/JDBI/PreparedStatement/executeUpdate";
  public static final String HIKARICP_CONNECTION_CLOSE = "HikariCP/Connection/close";
  public static final String POST_USERS_MENUSAUTHS = "POST:/users/menusAuths";
  public static final String GET_ZLB_GET_RURAL_COMMERCIAL_BANK_INFO = "GET:/zlb/getRuralCommercialBank/info";
  public static final String MYSQL_JDBI_CONNECTION_COMMIT = "Mysql/JDBI/Connection/commit";
  public static final String MYSQL_JDBI_PREPARED_STATEMENT_EXECUTE_QUERY = "Mysql/JDBI/PreparedStatement/executeQuery";
  public static final String SPRING_SCHEDULED = "SpringScheduled";
  public static final String POST_DEVICES_HEARTEAT = "POST:/devices/heartbeat";
  public static final Integer QUEUE_SIZE = 2000;
  public static final Integer RECORD_COUNT = 10 * 10000;
  public static final Long SLEEP_INTERVAL = 50L;
  public static final Integer INITIAL_PROCESSOR_THREAD_COUNT = 4;
  public static final Integer NUMBER_ZERO = 0;
  public static final Integer IO_THREAD_QUEUE_SIZE = 1000;
  public static final Integer INITIAL_FLUSH_TO_ROCKETMQ_INTERVAL = 5;
  public static final Integer FLUSH_TO_MQ_INTERVAL = 5;
  public static final Integer CURRENT_TIME_RANDOM = 30;
  public static final String KEYS_ALL = "keys *";
  public static final CharSequence LIKE = "like";
  public static final CharSequence IS_NOT_NULL = "IS NOT NULL";
  public static final CharSequence IS_NULL2 = "IS NULL";
  public static final Object LEFT_EXPRESSION = "leftExpression";
  public static final String IS = "IS";
  public static final Object OPERATE = "operate";
  public static final Object RIGHT_EXPRESSION = "rightExpression";
  public static final String ON = "on";
  public static final String OFF = "off";
  public static final String TRUE = "true";
  public static final String INIT_TIME = "1990-01-01 00:00:00";
  public static final Integer NUMBER_THREE = 3;

  private Const() {
    throw new IllegalStateException("Const class");
  }

  public static final String DB_TYPE = "db.type";
  public static final String DB_INSTANCE = "db.instance";
  public static final String DB_USER_NAME = "db_user_name";
  public static final String DB_STATEMENT = "db.statement";
  public static final String IS_NULL = "null";
  public static final String IP = "ip";
  public static final String CONTENT = "content";
  /**
   * 操作类型
   */
  public static final String OPERATION_TYPE_DING_TALK = "ding-talk";
  public static final String OPERATION_TYPE_DINGTALK = "dingtalk";
  public static final String OPERATION_TYPE_SQL = "sql";
  public static final String OPERATION_TYPE_URL = "url";
  public static final String OPERATION_TYPE_URL_NO_DB_STATEMENT = "url-no-statement";
  public static final String ADDREE = "address";

  public static final String USER_NAME = "userName";
  public static final String BODY = "body";
  public static final String HEADER = "header";
  public static final String REQUEST_START_TIME = "requestStartTime";
  public static final String PEER = "peer";
  public static final String SERVICE_INSTANCE_NAME = "serviceInstanceName";
  public static final String SERVICE_CODE = "serviceCode";
  public static final String DB_TYPE2 = "dbType";
  public static final String DB_TYPE_TIMES = "dbTypeTimes";
  public static final String DB_INSTANCE2 = "dbInstance";
  public static final String MS_TABLE_NAME = "msTableName";
  public static final String DB_USER_NAME2 = "dbUserName";
  public static final String DB_STATEMENT2 = "dbStatement";
  public static final String DB_TABLE_NAME = "dbTableName";
  public static final String TABLE_NAME = "tableName";
  public static final String TABLE_NAME_DESC = "tableNameDesc";
  public static final String DB_NAME = "dbName";
  public static final String DB_ADDRESS = "dbAddress";
  public static final String FUNCTION = "function";
  public static final String SEGMENTS = "segments";

  public static final String POUND_KEY = "#";
  public static final String COMMA = "#";
  public static final String COLON = "：";


  public static final String START_TIME = "startTime";
  public static final String END_TIME = "endTime";


  /**
   * 获取邮件的目的地址和发送内容
   */
  public static final String SEND_EMAIL = "send-email";

  public static final String FILE_OUTPUT = "file-output";

  public static final String OPERATION_TYPE_SELECT = "select";
  public static final String OPERATION_TYPE_UPDATE = "update";
  public static final String OPERATION_TYPE_DELETE = "delete";
  public static final String OPERATION_TYPE_INSERT = "insert";

  /**
   * 告警信息删除；
   */
  public static final String ANOMALY_DETECTION_INFO_DELETE = "delete";
  /**
   * 更新用户画像；
   */
  public static final String ANOMALY_DETECTION_INFO_UPDATE_USER_PORTRAIT = "update";

  /**
   * 记录每一天采集到多少条信息；2022-07-18 16:41:53  AllRecentSevenDays
   */
  public static final String HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS = "hash_everyday_ms_segment_detail_how_many_records";
  /*
   * 信息概览页面 ->用户访问行为；2022-07-19 08:52:11
   */
  /**
   * 统计每一个用户对数据库总的访问次数；
   */
  public static final String STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES = "string_user_access_behavior_all_visited_times#user_name:";
  /**
   * 记录用户对数据库最后的访问时间；
   */
  public static final String STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME = "string_user_access_behavior_latest_visited_time#user_name:";
  /**
   * 有序集合：存放的是用户对访问过的表的次数；2022-07-20 14:27:34
   */
  public static final String ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES = "zset_user_access_behavior_all_visited_tables#user_name#";
  /**
   * hash集合：存放的是每个表每天被访问的次数；2022-07-22 10:35:52
   */
  public static final String HASH_TABLE_EVERYDAY_VISITED_TIMES = "hash_table_everyday_visited_times#table_name#";
  /**
   * 有序集合：统计每个表操作类型次数；2022-07-22 15:47:48
   */
  public static final String ZSET_TABLE_OPERATION_TYPE = "zset_table_operation_type#table_name#";
  /**
   * 有序集合：统计每个用户操作类型次数；2022-07-22 15:47:48
   */
  public static final String ZSET_USER_OPERATION_TYPE = "zset_user_operation_type#user_name#";
  /*
   * 信息概览页面 -> 数据统计情况；2022-07-19 08:52:11
   */
  /**
   * 目前已收集到多少条 MsSegmentDetail 信息；
   */
  public static final String STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS = "string_data_statistics_how_many_ms_segment_detail_records";
  /**
   * 将所有的用户放入到set集合中；
   */
  public static final String SET_DATA_STATISTICS_HOW_MANY_USERS = "set_data_statistics_how_many_users";

  /**
   * 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
   */
  public static final String ZSET_TABLE_BY_HOW_MANY_USER_VISITED = "zset_table_by_how_many_user_visited#";
  /**
   * 记录每一个数据库表最后被访问的时间；
   */
  public static final String STRING_TABLE_LATEST_VISITED_TIME = "string_table_latest_visited_time#";

  public static final String LOGIN_FISH_EASIER = "login/fish/easier";
  public static final String ES_INDEX_SEGMENT_DETAIL = "segment_detail";

  public static final Integer IS_DELETE_ZERO = 0;
  public static final Integer IS_DELETE_ONE = 1;

  /**
   * 心跳间隔15分钟；2022-06-27 14:52:54
   */
  public static final Integer SKYWALKING_AGENT_HEART_BEAT_INTERVAL_SECONDS = 1 * 60;

  /**
   * IoThread线程间隔多久输出一次日志；2022-06-01 10:29:01
   */
  public static final Integer IOTREAD_LOG_INTERVAL = 50;

  public static final String SKYWALKING_AGENT_HEART_BEAT_DO_LIST = "string_skywalking_agent_heart_beat";
  public static final String SEGMENT_DETAIL_DO_LIST = "segmentDetaiDolList";
  public static final String SEGMENT_DETAIL_USERNAME_IS_NULL_DO_LIST = "segmentDetaiUserNameIsNullDolList";
  public static final String ABNORMAL = "abnormal";
  /**
   * 统计processor线程的QPS；2022-07-23 11:03:06
   */
  public static final String QPS_ZSET_EVERY_PROCESSOR_THREAD = "qps_zset_every_processor_thread_";
  /**
   * 统计所有Pocessor线程总的QPS；2022-07-27 10:15:21
   */
  public static final String QPS_ZSET_ALL_PROCESSOR_THREAD = "qps_zset_all_processor_thread";
  /**
   * 统计kafka消费者每秒钟拿到多少消息；2022-07-28 14:00:38
   */
  public static final String QPS_ZSET_KAFKA_CONSUMER_RECORDS_THREAD = "qps_zset_kafka_consumer_records_";
  /**
   * 统计第二层公共队列当前有多少个元素；2022-07-23 11:32:15
   */
  public static final String SECOND_QUEUE_SIZE_ZSET_BY_DISRUPTOR = "second_queue_size_zset_by_disruptor";
  public static final String SECOND_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE = "second_queue_size_zset_by_linked_blocking_queue";
  /**
   * 统计第一层队列当前有多少个元素；2022-07-26 17:21:20
   */
  public static final String FIRST_QUEUE_SIZE_ZSET_BY_DISRUPTOR = "first_queue_size_zset_by_disruptor";
  public static final String FIRST_QUEUE_SIZE_ZSET_BY_LINKED_BLOCKING_QUEUE = "first_queue_size_zset_by_linked_blocking_queue";
  public static final String SEGMENT = "segment";
  public static final String SPAN = "span";

  /**
   * 配置表中的配置项
   */
  public static final String AK_SK = "akSk";
  public static final String AK = "ak";
  public static final String SK = "sk";
  public static final String RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT = "sucess";

  public static final String SQL = "sql";
  public static final String SQL_SOURCE_DMS = "dms";
  public static final String SQL_SOURCE_INSIGHT = "sql_insight";
  public static final String SQL_SOURCE_SKYWALKING_AGENT = "skywalking_agent";

  public static final String SERVICE_ID_CONNECTOR = ".";
  public static final String SERVICE_ID_PARSER_SPLIT = "\\.";
  public static final String ID_CONNECTOR = "_";
  public static final String ID_PARSER_SPLIT = "\\_";
  public static final String RELATION_ID_CONNECTOR = "-";
  public static final String RELATION_ID_PARSER_SPLIT = "\\-";
  public static final String SEGMENT_SPAN_SPLIT = "S";

  public static final String DOLLAR = "$";

  /**
   * 判断skywalking Java探针发出的SQL语句的SQL类型；2022-05-28 12:56:57
   */
  public static final String SQL_TYPE_SELECT = "SELECT";
  public static final String SQL_TYPE_INSERT = "INSERT";
  public static final String SQL_TYPE_UPDATE = "UPDATE";
  public static final String SQL_TYPE_DELETE = "DELETE";
  public static final String SQL_TYPE_LOGIN = "LOGIN";
  public static final String SQL_TYPE_LOGOUT = "LOGOUT";
  public static final String SQL_TYPE_MERGE = "MERGE";
  public static final String SQL_TYPE_ALTER = "ALTER";
  public static final String SQL_TYPE_CREATEINDEX = "CREATEINDEX";
  public static final String SQL_TYPE_DROPINDEX = "DROPINDEX";
  public static final String SQL_TYPE_CREATE = "CREATE";
  public static final String SQL_TYPE_DROP = "DROP";
  public static final String SQL_TYPE_SET = "SET";
  public static final String SQL_TYPE_DESC = "DESC";
  public static final String SQL_TYPE_REPLACE = "REPLACE";
  public static final String SQL_TYPE_CALL = "CALL";
  public static final String SQL_TYPE_BEGIN = "BEGIN";
  public static final String SQL_TYPE_DESCRIBE = "DESCRIBE";
  public static final String SQL_TYPE_ROLLBACK = "ROLLBACK";
  public static final String SQL_TYPE_FLUSH = "FLUSH";
  public static final String SQL_TYPE_USE = "USE";
  public static final String SQL_TYPE_SHOW = "SHOW";
  public static final String SQL_TYPE_START = "START";
  public static final String SQL_TYPE_COMMIT = "COMMIT";
  public static final String SQL_TYPE_RENAME = "RENAME";
}
