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

  public static final String LOGIN_FISH_EASIER = "login/fish/easier";
  public static final String ES_INDEX_SEGMENT_DETAIL = "segment_detail";

  public static final Integer IS_DELETE_ZERO = 0;
  public static final Integer IS_DELETE_ONE = 1;

  // 心跳间隔15分钟；2022-06-27 14:52:54
  public static final Integer SKYWALKING_AGENT_HEART_BEAT_INTERVAL_MINUTES = 15;
  public static final Integer SKYWALKING_AGENT_HEART_BEAT_INTERVAL_SECONDS = 15 * 60;

  // IoThread线程间隔多久输出一次日志；2022-06-01 10:29:01
  public static final Integer IOTREAD_LOG_INTERVAL = 50;
  // public static final String AUDITLOG_FROM_SKYWALKING_AGENT_LIST = "auditLogFromSkywalkingAgentList";

  // 将信息概况页面中的数据访问行为存储到Redis中；2022-07-18 16:15:07

  // 近七天信息采集情况；2022-07-18 16:41:53  AllRecentSevenDays
  public static final String ALL_RECENT_SEVEN_DAYS_MS_SEGMENT_DETAIL_STATISTICS = "all_recent_seven_days_ms_segment_detail_statistics";

  /*
   * 信息概览页面 ->用户访问行为；2022-07-19 08:52:11
   */
  // 用户对数据库表总的访问次数；
  public static final String USER_ACCESS_BEHAVIOR_USER_NAME_VISITED_COUNT = "user_access_behavior_visited_count#user_name:";
  // 用户对数据库表最后的访问时间；
  public static final String USER_ACCESS_BEHAVIOR_USER_NAME_LATEST_VISITED_TIME = "user_access_behavior_latest_visited_time#user_name:";

  public static final String ZSET_USER_ACCESS_BEHAVIOR_USER_NAME = "zset_user_access_behavior#user_name#";

  /*
   * 信息概览页面 -> 数据统计情况；2022-07-19 08:52:11
   */
  // 数据统计在Redis中的哈希表名称；
  // 已收集信息；
  public static final String DATA_STATISTICS_ALL_MS_SEGMENT_DETAIL = "data_statistics_all_ms_segment_detail";
  // 用户人数；
  public static final String DATA_STATISTICS_USER_COUNT = "data_statistics_user_count";


  public static final String COARSE_STATISTICS_VISITED_TABLE = "coarse_statistics_visited_table";
  // 每天采集信息统计；2022-07-18 16:16:10
  public static final String STATISTICS_EVERYDAY_TABLE = "statistics_everyday_table";

  public static final String SKYWALKING_AGENT_HEART_BEAT_DO_LIST = "skywalkingAgentHeartBeatDolList";
  public static final String SKYWALKING_CONSUME_QPS = "skywalkingConsumeQps";
  public static final String SEGMENT_DETAIL_DO_LIST = "segmentDetaiDolList";
  public static final String ES_SEGMENT_DETAIL_DO_LIST = "esSegmentDetaiDolList";
  public static final String ABNORMAL = "abnormal";
  public static final String SEGMENT = "segment";
  public static final String SPAN = "span";
  // public static final String SEGMENT_LIST = "segmentList";

  // 配置表中的配置项
  public static final String AK_SK = "akSk";
  public static final String AK = "ak";
  public static final String SK = "sk";
  public static final String RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT = "sucess";
  public static final String RETCH_AUDIT_LOG_BY_DMS_FAILURE_RESULT = "failure";

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
