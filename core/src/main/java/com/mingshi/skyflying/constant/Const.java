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

  public static final Integer IS_DELETE_ZERO = 0;
  public static final Integer IS_DELETE_ONE = 1;

  // 心跳间隔15分钟；2022-06-27 14:52:54
  public static final Integer SKYWALKING_AGENT_HEART_BEAT_INTERVAL = 15;

  // IoThread线程间隔多久输出一次日志；2022-06-01 10:29:01
  public static final Integer IOTREAD_LOG_INTERVAL = 50;
  public static final String AUDITLOG_FROM_SKYWALKING_AGENT_LIST = "auditLogFromSkywalkingAgentList";
  public static final String SKYWALKING_AGENT_HEART_BEAT_DO_LIST = "skywalkingAgentHeartBeatDolList";
  public static final String SEGMENT_DETAIL_DO_LIST = "segmentDetaiDolList";
  public static final String ABNORMAL = "abnormal";
  // public static final String SEGMENT = "segment";
  public static final String SEGMENT_LIST = "segmentList";

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
