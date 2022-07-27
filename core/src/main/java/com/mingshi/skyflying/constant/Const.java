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

package com.mingshi.skyflying.constant;

public class Const {

  // 告警信息删除；
  public static final String ANOMALY_DETECTION_INFO_DELETE = "delete";
  // 更新用户画像；
  public static final String ANOMALY_DETECTION_INFO_UPDATE_USER_PORTRAIT = "update";

  // 目前在缓存里，缓存的数据有：
  // 记录每一天采集到多少条信息；2022-07-18 16:41:53  AllRecentSevenDays
  public static final String HASH_EVERYDAY_MS_SEGMENT_DETAIL_HOW_MANY_RECORDS = "hash_everyday_ms_segment_detail_how_many_records";
  /*
   * 信息概览页面 ->用户访问行为；2022-07-19 08:52:11
   */
  // 统计每一个用户对数据库总的访问次数；
  public static final String STRING_USER_ACCESS_BEHAVIOR_ALL_VISITED_TIMES = "string_user_access_behavior_all_visited_times#user_name:";
  // 记录用户对数据库最后的访问时间；
  public static final String STRING_USER_ACCESS_BEHAVIOR_LATEST_VISITED_TIME = "string_user_access_behavior_latest_visited_time#user_name:";
  // 有序集合：存放的是用户对访问过的表的次数；2022-07-20 14:27:34
  public static final String ZSET_USER_ACCESS_BEHAVIOR_ALL_VISITED_TABLES = "zset_user_access_behavior_all_visited_tables#user_name#";
  // hash集合：存放的是每个表每天被访问的次数；2022-07-22 10:35:52
  public static final String HASH_TABLE_EVERYDAY_VISITED_TIMES = "hash_table_everyday_visited_times#table_name#";
  // 有序集合：统计每个表操作类型次数；2022-07-22 15:47:48
  public static final String ZSET_TABLE_OPERATION_TYPE = "zset_table_operation_type#table_name#";
  // 有序集合：统计每个用户操作类型次数；2022-07-22 15:47:48
  public static final String ZSET_USER_OPERATION_TYPE = "zset_user_operation_type#user_name#";
  /*
   * 信息概览页面 -> 数据统计情况；2022-07-19 08:52:11
   */
  // 目前已收集到多少条 MsSegmentDetail 信息；
  public static final String STRING_DATA_STATISTICS_HOW_MANY_MS_SEGMENT_DETAIL_RECORDS = "string_data_statistics_how_many_ms_segment_detail_records";
  // 将所有的用户放入到set集合中；
  public static final String SET_DATA_STATISTICS_HOW_MANY_USERS = "set_data_statistics_how_many_users";

  // 有序集合，统计一个表被哪些用户访问的次数；2022-07-20 15:39:57
  public static final String ZSET_TABLE_BY_HOW_MANY_USER_VISITED = "zset_table_by_how_many_user_visited#";
  // 记录每一个数据库表最后被访问的时间；
  public static final String STRING_TABLE_LATEST_VISITED_TIME = "string_table_latest_visited_time#";

  public static final String LOGIN_FISH_EASIER = "login/fish/easier";
  public static final String ES_INDEX_SEGMENT_DETAIL = "segment_detail";

  public static final Integer IS_DELETE_ZERO = 0;
  public static final Integer IS_DELETE_ONE = 1;

  // 心跳间隔15分钟；2022-06-27 14:52:54
  public static final Integer SKYWALKING_AGENT_HEART_BEAT_INTERVAL_SECONDS = 15 * 60;

  // IoThread线程间隔多久输出一次日志；2022-06-01 10:29:01
  public static final Integer IOTREAD_LOG_INTERVAL = 50;

  public static final String SKYWALKING_AGENT_HEART_BEAT_DO_LIST = "string_skywalking_agent_heart_beat";
  public static final String SEGMENT_DETAIL_DO_LIST = "segmentDetaiDolList";
  public static final String ES_SEGMENT_DETAIL_DO_LIST = "esSegmentDetaiDolList";
  public static final String ABNORMAL = "abnormal";
  // 统计processor线程的QPS；2022-07-23 11:03:06
  public static final String ZSET_PROCESSOR_THREAD_QPS = "zset_processor_thread_qps_";
  // 统计IoThread队列的大小；2022-07-23 12:41:17
  public static final String ZSET_IO_THREAD_QUEUE_SIZE = "zset_io_thread_queue_size";
  // 统计第二层公共队列当前有多少个元素；2022-07-23 11:32:15
  public static final String ZSET_IO_THREAD_BATCH_INSERT_LINKED_BLOCKING_QUEUE_ZISE = "zset_io_thread_batch_insert_linked_blocking_queue_size";
  // 统计第一层队列当前有多少个元素；2022-07-26 17:21:20
  public static final String ZSET_ACCEPTOR_THREAD_QUEUE_ZISE = "zset_acceptor_thread_queue_size";
  public static final String SEGMENT = "segment";
  public static final String SPAN = "span";

  // 配置表中的配置项
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

  // 判断skywalking Java探针发出的SQL语句的SQL类型；2022-05-28 12:56:57
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
