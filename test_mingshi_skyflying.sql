/*
 Navicat MySQL Data Transfer

 Source Server         : 新-MySQL8-46
 Source Server Type    : MySQL
 Source Server Version : 80027
 Source Host           : 10.0.107.46:3306
 Source Schema         : test_mingshi_skyflying

 Target Server Type    : MySQL
 Target Server Version : 80027
 File Encoding         : 65001

 Date: 15/09/2022 17:59:13
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for coarse_segment_detail_on_time
-- ----------------------------
DROP TABLE IF EXISTS `coarse_segment_detail_on_time`;
CREATE TABLE `coarse_segment_detail_on_time` (
  `id` int NOT NULL AUTO_INCREMENT,
  `username` varchar(256) COLLATE utf8mb4_bin NOT NULL,
  `counts` int DEFAULT '0' COMMENT '当天该用户访问总次数',
  `time_interval_count_0_1` int DEFAULT '0' COMMENT '0点到1点访问次数',
  `time_interval_count_1_2` int DEFAULT '0' COMMENT '1点到2点访问次数',
  `time_interval_count_2_3` int DEFAULT '0' COMMENT '2点到3点访问次数',
  `time_interval_count_3_4` int DEFAULT '0' COMMENT '3点到4点访问次数',
  `time_interval_count_4_5` int DEFAULT '0' COMMENT '4点到5点访问次数',
  `time_interval_count_5_6` int DEFAULT '0' COMMENT '5点到6点访问次数',
  `time_interval_count_6_7` int DEFAULT '0' COMMENT '6点到7点访问次数',
  `time_interval_count_7_8` int DEFAULT '0' COMMENT '7点到8点访问次数',
  `time_interval_count_8_9` int DEFAULT '0' COMMENT '8点到9点访问次数',
  `time_interval_count_9_10` int DEFAULT '0' COMMENT '9点到10点访问次数',
  `time_interval_count_10_11` int DEFAULT '0' COMMENT '10点到11点访问次数',
  `time_interval_count_11_12` int DEFAULT '0' COMMENT '11点到12点访问次数',
  `time_interval_count_12_13` int DEFAULT '0' COMMENT '12点到13点访问次数',
  `time_interval_count_13_14` int DEFAULT '0' COMMENT '13点到14点访问次数',
  `time_interval_count_14_15` int DEFAULT '0' COMMENT '14点到15点访问次数',
  `time_interval_count_15_16` int DEFAULT '0' COMMENT '15点到16点访问次数',
  `time_interval_count_16_17` int DEFAULT '0' COMMENT '16点到17点访问次数',
  `time_interval_count_17_18` int DEFAULT '0' COMMENT '17点到18点访问次数',
  `time_interval_count_18_19` int DEFAULT '0' COMMENT '18点到19点访问次数',
  `time_interval_count_19_20` int DEFAULT '0' COMMENT '19点到20点访问次数',
  `time_interval_count_20_21` int DEFAULT '0' COMMENT '20点到21点访问次数',
  `time_interval_count_21_22` int DEFAULT '0' COMMENT '21点到22点访问次数',
  `time_interval_count_22_23` int DEFAULT '0' COMMENT '22点到23点访问次数',
  `time_interval_count_23_24` int DEFAULT '0' COMMENT '23点到24点访问次数',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `update_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `is_deleted` int NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `coarse_segment_detail_on_time_id_uindex` (`id`),
  KEY `coarse_segment_detail_on_time_create_time_index` (`create_time`),
  KEY `coarse_segment_detail_on_time_username_index` (`username`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='基于时间维度的粗粒度中间表';

-- ----------------------------
-- Table structure for ms_agent_information
-- ----------------------------
DROP TABLE IF EXISTS `ms_agent_information`;
CREATE TABLE `ms_agent_information` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `agent_code` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '运维人员在服务启动时，设置的名称',
  `agent_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '监管方或者运维人员设置的服务名称',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_ agent_code` (`agent_code`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=33 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='探针信息表';

-- ----------------------------
-- Records of ms_agent_information
-- ----------------------------
BEGIN;
INSERT INTO `ms_agent_information` VALUES (1, 0, '2022-09-15 09:59:11', '2022-09-15 09:59:11', 'hy2-api-gateway-test', NULL);
INSERT INTO `ms_agent_information` VALUES (2, 0, '2022-09-15 09:59:11', '2022-09-15 09:59:11', 'demo-application-springboot', NULL);
INSERT INTO `ms_agent_information` VALUES (3, 0, '2022-09-15 09:59:11', '2022-09-15 09:59:11', 'hy2-company-test', NULL);
INSERT INTO `ms_agent_information` VALUES (4, 0, '2022-09-15 09:59:11', '2022-09-15 09:59:11', 'firewall-management-01-Djava.security.egd=file:/de', NULL);
COMMIT;

-- ----------------------------
-- Table structure for ms_agent_switch
-- ----------------------------
DROP TABLE IF EXISTS `ms_agent_switch`;
CREATE TABLE `ms_agent_switch` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `operation_type` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作类型；查询、探针的开关',
  `service_instance` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '探针唯一标识',
  `agent_switch_status` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '探针开启、关闭标识',
  `send_kafka_status` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '发送到kafka服务端的状态：失败还是成功',
  `receive_kafka_status` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '探针执行成功与否',
  `operator` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作人员',
  `send_kafka_request_params` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '发送到kafka服务端的请求参数',
  `receive_kafka_response_params` varchar(255) DEFAULT NULL COMMENT '探针发来的响应信息',
  `request_id` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '本地操作的唯一标识',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_request_id` (`request_id`) USING BTREE,
  KEY `idx_service_instance` (`service_instance`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='探针关闭开启记录表';

-- ----------------------------
-- Table structure for ms_alarm_information
-- ----------------------------
DROP TABLE IF EXISTS `ms_alarm_information`;
CREATE TABLE `ms_alarm_information` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `match_rule_id` int DEFAULT NULL COMMENT '命中规则id',
  `original_time` datetime DEFAULT NULL COMMENT '记录产生的时间',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '用户名',
  `alarm_content` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '告警内容',
  `global_trace_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL COMMENT '全局追踪id',
  `update_user_portrait` tinyint DEFAULT '0' COMMENT '更新用户画像；0：未更新；1-已更新；',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='告警信息表';

-- ----------------------------
-- Table structure for ms_config
-- ----------------------------
DROP TABLE IF EXISTS `ms_config`;
CREATE TABLE `ms_config` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `config` varchar(255) DEFAULT NULL COMMENT '配置项',
  `config_type` varchar(16) DEFAULT NULL COMMENT '配置类型',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='系统配置表';

-- ----------------------------
-- Records of ms_config
-- ----------------------------
BEGIN;
INSERT INTO `ms_config` VALUES (1, 0, '2022-05-26 07:53:21', '2022-05-26 07:53:21', '{\"ak\":\"LTAI5tBM5srwtPdYWpngoJCW\",\"sk\":\"068ZDt4f324GR9muOpmdhxoeIe4FIV\"}', 'akSk');
COMMIT;

-- ----------------------------
-- Table structure for ms_dms_audit_log
-- ----------------------------
DROP TABLE IF EXISTS `ms_dms_audit_log`;
CREATE TABLE `ms_dms_audit_log` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `application_user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户登录系统的用户名',
  `global_trace_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前这条SQL对应的全局追踪id',
  `sql_insight_db_user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'sql洞察获取到的数据库名称',
  `ms_table_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库表名',
  `sql_source` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '来源；是来自探针，还是来自其他的访问方式；',
  `ms_sql` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '已执行的SQL语句',
  `op_time` varchar(32) DEFAULT NULL COMMENT '操作时间',
  `user_name` varchar(32) DEFAULT NULL COMMENT '操作人昵称',
  `instance_name` varchar(128) DEFAULT NULL COMMENT '数据库名称',
  `instance_id` int DEFAULT NULL COMMENT '实例ID',
  `ms_schema_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库名称',
  `db_id` int DEFAULT NULL COMMENT '数据库ID',
  `logic` varchar(16) DEFAULT NULL COMMENT '是否为逻辑库，返回值说明如下：\n\ntrue：逻辑库\nfalse：物理库',
  `sql_type` varchar(16) DEFAULT NULL COMMENT 'SQL类型，常见SQL类型返回值说明如下：\n\nSELECT：查询\nINSERT：插入\nDELETE：删除\nCREATE_TABLE：创建表',
  `exec_state` varchar(16) DEFAULT NULL COMMENT '执行状态，返回值说明如下：\n\nFAIL：执行失败\nNOEXE：暂未执行\nRUNNING：执行中\nCANCEL：取消执行\nSUCCESS：执行成功',
  `affect_rows` int DEFAULT NULL COMMENT '表示执行SQL语句后返回的总行数。比如，SQL语句为查询语句，此参数返回的是查询数据的总行数。',
  `elapsed_time` int DEFAULT NULL COMMENT '操作耗时，单位为毫秒。',
  `remark` varchar(255) DEFAULT NULL COMMENT '备注信息',
  `user_id` int DEFAULT NULL COMMENT '操作人ID',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `hash` varchar(128) DEFAULT NULL COMMENT '本条记录的hash值',
  `sql_insight_user_ip` varchar(128) DEFAULT NULL COMMENT 'sql洞察记录的访问实例的IP',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_hash` (`hash`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='数据库审计日志表';

-- ----------------------------
-- Table structure for ms_monitor_business_system_tables
-- ----------------------------
DROP TABLE IF EXISTS `ms_monitor_business_system_tables`;
CREATE TABLE `ms_monitor_business_system_tables` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `system_name` varchar(128) DEFAULT NULL COMMENT '业务系统名称',
  `table_name` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '表名',
  `db_name` varchar(128) DEFAULT NULL COMMENT '数据库名称',
  `db_address` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库地址',
  `table_desc` varchar(64) DEFAULT NULL COMMENT '表中存储什么类型的数据',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_table_name` (`table_name`,`db_name`,`db_address`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='监管业务系统数据库表';

-- ----------------------------
-- Table structure for ms_scheduled_task
-- ----------------------------
DROP TABLE IF EXISTS `ms_scheduled_task`;
CREATE TABLE `ms_scheduled_task` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `start_time` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '开始时间',
  `end_time` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '结束时间',
  `page_size` int DEFAULT NULL COMMENT '每页数据量',
  `page_number` int DEFAULT NULL COMMENT '开始页码',
  `status` varchar(16) DEFAULT NULL COMMENT '执行结果',
  `record_count` int DEFAULT NULL COMMENT '本地拉取到的数据量',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='定时拉取DMS审计日志任务表';

-- ----------------------------
-- Table structure for ms_segment_detail
-- ----------------------------
DROP TABLE IF EXISTS `ms_segment_detail`;
CREATE TABLE `ms_segment_detail` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
  `user_login_ip` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'ip地址',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的用户名',
  `service_code` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当应用系统与探针一起启动时，操作人员设置的应用系统名称',
  `start_time` datetime DEFAULT NULL COMMENT '请求开始时间',
  `operation_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的URL',
  `token` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'token',
  `global_trace_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '全局traceId',
  `db_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '对数据库的操作类型，select：查询、insert：插入、update：更新、delete：删除',
  `db_user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库用户名',
  `parent_segment_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前segment的父segmentId',
  `operation_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作类型；是访问Redis、MySQL或远程调用等',
  `ms_table_name` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库表名',
  `db_instance` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库名称',
  `db_statement` longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '具体的SQL语句',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `span_id` int DEFAULT NULL COMMENT 'spanid',
  `component` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '拦截插件的名称',
  `peer` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '目的地址，比如当前微服务调用其他微服务的地址',
  `endpoint_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '拦截方法的名称',
  `service_instance_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库连接地址',
  `end_time` datetime DEFAULT NULL COMMENT '请求结束的时间',
  `user_portrait_flag_by_visited_time` tinyint DEFAULT '0' COMMENT '基于访问时间维度的用户画像；0：未标识；1：已标识过；',
  `user_portrait_flag_by_visited_table_everyday` tinyint DEFAULT '0' COMMENT '基于访问过的表维度的用户画像；0：未标识；1：已标识过；',
  `parent_span_id` int DEFAULT NULL COMMENT '父span的id',
  `current_segment_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前segment的id',
  PRIMARY KEY (`id`),
  KEY `idx_global_trace_id` (`global_trace_id`) USING BTREE,
  KEY `idx_token` (`token`) USING BTREE,
  KEY `idx_user_name_token_global_trace_id` (`user_name`,`token`,`global_trace_id`) USING BTREE,
  KEY `idx_table_name_db_instance_peer` (`ms_table_name`,`db_instance`,`peer`) USING BTREE,
  KEY `idx_start_time` (`start_time`) USING BTREE,
  KEY `idx_db_type` (`db_type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户操作记录详情表';

-- ----------------------------
-- Table structure for ms_segment_detail_username_is_null
-- ----------------------------
DROP TABLE IF EXISTS `ms_segment_detail_username_is_null`;
CREATE TABLE `ms_segment_detail_username_is_null` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
  `user_login_ip` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'ip地址',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的用户名',
  `service_code` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当应用系统与探针一起启动时，操作人员设置的应用系统名称',
  `start_time` datetime DEFAULT NULL COMMENT '请求开始时间',
  `db_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '对数据库的操作类型，select：查询、insert：插入、update：更新、delete：删除',
  `db_user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库用户名',
  `operation_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的URL',
  `token` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'token',
  `global_trace_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '全局traceId',
  `parent_segment_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前segment的父segmentId',
  `operation_type` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作类型；是访问Redis、MySQL或远程调用等',
  `ms_table_name` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库表名',
  `db_instance` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库名称',
  `db_statement` longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '具体的SQL语句',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `span_id` int DEFAULT NULL COMMENT 'spanid',
  `component` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '拦截插件的名称',
  `peer` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '目的地址，比如当前微服务调用其他微服务的地址',
  `endpoint_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '拦截方法的名称',
  `service_instance_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库连接地址',
  `end_time` datetime DEFAULT NULL COMMENT '请求结束的时间',
  `parent_span_id` int DEFAULT NULL COMMENT '父span的id',
  `current_segment_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前segment的id',
  PRIMARY KEY (`id`),
  KEY `idx_global_trace_id` (`global_trace_id`) USING BTREE,
  KEY `idx_token` (`token`) USING BTREE,
  KEY `idx_user_name_token_global_trace_id` (`user_name`,`token`,`global_trace_id`) USING BTREE,
  KEY `idx_table_name_db_instance_peer` (`ms_table_name`,`db_instance`,`peer`) USING BTREE,
  KEY `idx_start_time` (`start_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户操作记录用户名为空的表';

-- ----------------------------
-- Table structure for operate_log
-- ----------------------------
DROP TABLE IF EXISTS `operate_log`;
CREATE TABLE `operate_log` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `operation_desc` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作描述',
  `method_name` varchar(64) NOT NULL DEFAULT '' COMMENT '处理请求的接口',
  `order_id` varchar(64) NOT NULL COMMENT '订单号',
  `user_name` varchar(32) NOT NULL DEFAULT '' COMMENT '用户名',
  `login_ip` varchar(64) NOT NULL DEFAULT '' COMMENT '发出请求的ip',
  `request_url` varchar(128) NOT NULL DEFAULT '' COMMENT '请求的URL',
  `request_params` varchar(128) NOT NULL DEFAULT '' COMMENT '请求的参数',
  `response_params` longtext COMMENT '请求处理的结果',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `used_time` int DEFAULT '0' COMMENT '调用接口用时（单位，毫秒）',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `idx_order_id` (`order_id`) USING BTREE,
  KEY `idx_userName` (`user_name`) USING BTREE,
  KEY `idx_methodName` (`method_name`) USING BTREE,
  KEY `idx_loginIp` (`login_ip`) USING BTREE,
  KEY `idxe_gmtCreate` (`gmt_create`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户操作记录表';

-- ----------------------------
-- Table structure for segment
-- ----------------------------
DROP TABLE IF EXISTS `segment`;
CREATE TABLE `segment` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的用户名',
  `token` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的token',
  `global_trace_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '全局traceId',
  `current_segment_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前segment的id',
  `parent_segment_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前segment的父segmentId',
  `operation_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作的URL',
  `request_start_time` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT '' COMMENT '请求开始时间',
  `reorganizing_spans` longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '重组后的调用链信息，给前端用的',
  `spans` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '完整的调用链信息',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  PRIMARY KEY (`id`),
  KEY `idx_global_trace_id` (`global_trace_id`) USING BTREE,
  KEY `idx_user_name` (`user_name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户操作原始信息表';

-- ----------------------------
-- Table structure for sys_menu
-- ----------------------------
DROP TABLE IF EXISTS `sys_menu`;
CREATE TABLE `sys_menu` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `is_delete` tinyint NOT NULL DEFAULT '0' COMMENT '是否删除状态，1：删除，0：有效',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `gmt_modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  `creator` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建人',
  `type` tinyint NOT NULL DEFAULT '2' COMMENT '类型   1:系统 2：父菜单  3：菜单（叶子节点）',
  `title` varchar(16) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT '' COMMENT '菜单名称',
  `href` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单地址',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='菜单表';

-- ----------------------------
-- Records of sys_menu
-- ----------------------------
BEGIN;
INSERT INTO `sys_menu` VALUES (1, 0, '2022-09-09 00:58:48', '2022-09-09 00:58:48', 'admin', 2, '信息概况', '[\'/api/skyflying/getCoarseCountsOfUser\',\'/api/skyflying/getCountsOfAllRecentSevenDays\',\'/api/skyflying/getCoarseCountsOfTableName\',\'/api/skyflying/getOverviewOfSystem\',\'/api/skyflying/getAllAlarmInfoDetailByUserName\',\'\']');
INSERT INTO `sys_menu` VALUES (2, 0, '2022-09-09 00:58:48', '2022-09-09 00:58:48', 'admin', 2, '用户行为', '[\'/api/skyflying/getCoarseCountsOfUser\']');
INSERT INTO `sys_menu` VALUES (3, 0, '2022-09-09 00:58:49', '2022-09-09 00:58:49', 'admin', 2, '数据分布', '[\'/api/skyflying/getCoarseCountsOfTableName\']');
INSERT INTO `sys_menu` VALUES (4, 0, '2022-09-09 00:58:50', '2022-09-09 00:58:50', 'admin', 2, '告警信息', '[\'/api/skyflying/getAlarmData\',\'/api/skyflying/getUserAlarmData\',\'api/skyflying/getAnomalyDetectionInfoByGroupByUserName\']');
INSERT INTO `sys_menu` VALUES (5, 0, '2022-09-09 00:58:50', '2022-09-09 00:58:50', 'admin', 2, '库表管理', '[\'/api/skyflying/getAllMonitorTables\']');
INSERT INTO `sys_menu` VALUES (6, 0, '2022-09-09 00:58:51', '2022-09-09 00:58:51', 'admin', 2, '检测规则', '[\'api/skyflying/getUserPortraitRules\']');
INSERT INTO `sys_menu` VALUES (7, 0, '2022-09-09 00:58:52', '2022-09-09 00:58:52', 'admin', 2, '服务与探针', '[\'/api/skyflying/getAllSkywalkingAgent\']');
INSERT INTO `sys_menu` VALUES (8, 0, '2022-09-15 09:25:38', '2022-09-15 09:25:38', 'admin', 2, '操作审计', '[\'/api/skyflying/getHighDangerOperationLog\']');
COMMIT;

-- ----------------------------
-- Table structure for sys_menu_role
-- ----------------------------
DROP TABLE IF EXISTS `sys_menu_role`;
CREATE TABLE `sys_menu_role` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `is_delete` tinyint NOT NULL DEFAULT '0' COMMENT '是否删除状态，1：删除，0：有效',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `gmt_modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  `creator` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '创建人',
  `modifier` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '最后修改人',
  `menu_id` bigint NOT NULL COMMENT '菜单id',
  `role_id` bigint NOT NULL COMMENT '角色id',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=31 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户角色表';

-- ----------------------------
-- Records of sys_menu_role
-- ----------------------------
BEGIN;
INSERT INTO `sys_menu_role` VALUES (1, 0, '2022-09-09 01:05:29', '2022-09-09 01:05:29', 'admin', 'admin', 1, 1);
INSERT INTO `sys_menu_role` VALUES (2, 0, '2022-09-09 01:05:30', '2022-09-09 01:05:30', 'admin', 'admin', 2, 1);
INSERT INTO `sys_menu_role` VALUES (3, 0, '2022-09-09 01:05:31', '2022-09-09 01:05:31', 'admin', 'admin', 3, 1);
INSERT INTO `sys_menu_role` VALUES (4, 0, '2022-09-09 01:05:32', '2022-09-09 01:05:32', 'admin', 'admin', 4, 1);
INSERT INTO `sys_menu_role` VALUES (5, 0, '2022-09-09 01:05:32', '2022-09-09 01:05:32', 'admin', 'admin', 5, 1);
INSERT INTO `sys_menu_role` VALUES (6, 0, '2022-09-09 01:05:33', '2022-09-09 01:05:33', 'admin', 'admin', 6, 1);
INSERT INTO `sys_menu_role` VALUES (7, 0, '2022-09-09 01:05:34', '2022-09-09 01:05:34', 'admin', 'admin', 7, 1);
INSERT INTO `sys_menu_role` VALUES (8, 0, '2022-09-09 01:05:35', '2022-09-09 01:05:35', 'admin', 'admin', 1, 2);
INSERT INTO `sys_menu_role` VALUES (9, 0, '2022-09-09 01:05:36', '2022-09-09 01:05:36', 'admin', 'admin', 2, 2);
INSERT INTO `sys_menu_role` VALUES (10, 0, '2022-09-09 01:05:39', '2022-09-09 01:05:39', 'admin', 'admin', 3, 2);
INSERT INTO `sys_menu_role` VALUES (11, 0, '2022-09-09 01:05:40', '2022-09-09 01:05:40', 'admin', 'admin', 4, 2);
INSERT INTO `sys_menu_role` VALUES (12, 0, '2022-09-09 01:05:41', '2022-09-09 01:05:41', 'admin', 'admin', 5, 2);
INSERT INTO `sys_menu_role` VALUES (13, 0, '2022-09-09 01:05:43', '2022-09-09 01:05:43', 'admin', 'admin', 6, 2);
INSERT INTO `sys_menu_role` VALUES (14, 0, '2022-09-09 01:05:44', '2022-09-09 01:05:44', 'admin', 'admin', 7, 2);
INSERT INTO `sys_menu_role` VALUES (15, 0, '2022-09-15 09:31:18', '2022-09-15 09:31:18', 'admin', 'admin', 8, 1);
INSERT INTO `sys_menu_role` VALUES (16, 0, '2022-09-15 09:31:28', '2022-09-15 09:31:28', 'admin', 'admin', 8, 2);
COMMIT;

-- ----------------------------
-- Table structure for sys_operator
-- ----------------------------
DROP TABLE IF EXISTS `sys_operator`;
CREATE TABLE `sys_operator` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `is_delete` tinyint NOT NULL DEFAULT '0' COMMENT '是否删除状态，1：删除，0：有效',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `gmt_modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  `creator` varchar(32) DEFAULT NULL COMMENT '创建人',
  `modifier` varchar(32) DEFAULT NULL COMMENT '最后修改人',
  `status` tinyint NOT NULL DEFAULT '1' COMMENT '状态 1:正常 2:冻结 3:离职',
  `user_name` varchar(32) NOT NULL COMMENT '用户名',
  `password` varchar(64) NOT NULL COMMENT '密码',
  `salt` varchar(32) NOT NULL COMMENT '随机salt值',
  `phone` varchar(32) DEFAULT '' COMMENT '电话',
  `name` varchar(32) DEFAULT NULL COMMENT '姓名',
  `email` varchar(64) DEFAULT '' COMMENT '邮箱',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=13 DEFAULT CHARSET=utf8mb3 COMMENT='系统用户信息表';

-- ----------------------------
-- Records of sys_operator
-- ----------------------------
BEGIN;
INSERT INTO `sys_operator` VALUES (1, 0, '2021-07-27 10:25:34', '2021-07-27 10:25:34', 'admin', NULL, 1, 'admin', 'de6eb79b18bd0c2fcbb3ce1d50f1b5fddb684409', '884df31e2e88e12a', '', NULL, '');
INSERT INTO `sys_operator` VALUES (2, 0, '2022-09-09 00:34:02', '2022-09-09 00:34:02', 'admin', NULL, 1, 'audit', 'de6eb79b18bd0c2fcbb3ce1d50f1b5fddb684409', '884df31e2e88e12a', '', NULL, '');
COMMIT;

-- ----------------------------
-- Table structure for sys_operator_role
-- ----------------------------
DROP TABLE IF EXISTS `sys_operator_role`;
CREATE TABLE `sys_operator_role` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `is_delete` tinyint NOT NULL DEFAULT '0' COMMENT '是否删除状态，1：删除，0：有效',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `gmt_modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  `creator` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '创建人',
  `modifier` varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci DEFAULT NULL COMMENT '最后修改人',
  `operator_id` bigint NOT NULL COMMENT '用户id',
  `role_id` bigint NOT NULL DEFAULT '2' COMMENT '角色id',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户角色表';

-- ----------------------------
-- Records of sys_operator_role
-- ----------------------------
BEGIN;
INSERT INTO `sys_operator_role` VALUES (1, 0, '2021-06-08 16:43:19', '2021-06-08 16:43:19', 'admin', NULL, 1, 1);
INSERT INTO `sys_operator_role` VALUES (2, 0, '2021-06-08 16:43:30', '2021-06-08 16:43:30', 'admin', NULL, 2, 2);
COMMIT;

-- ----------------------------
-- Table structure for sys_role
-- ----------------------------
DROP TABLE IF EXISTS `sys_role`;
CREATE TABLE `sys_role` (
  `id` bigint NOT NULL AUTO_INCREMENT COMMENT '主键id',
  `is_delete` tinyint NOT NULL DEFAULT '0' COMMENT '是否删除状态，1：删除，0：有效',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `gmt_modified` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '最后修改时间',
  `creator` varchar(32) NOT NULL COMMENT '创建人',
  `modifier` varchar(32) DEFAULT NULL COMMENT '最后修改人',
  `role_type` int NOT NULL COMMENT '类型  0:超级管理员 1:审计人员',
  `role_name` varchar(32) NOT NULL COMMENT '角色名称',
  `read_only` tinyint NOT NULL DEFAULT '0' COMMENT '是否只读；0：可读可写；1：只读模式',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `UK_idx` (`creator`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb3 COMMENT='系统用户信息表';

-- ----------------------------
-- Records of sys_role
-- ----------------------------
BEGIN;
INSERT INTO `sys_role` VALUES (1, 0, '2021-06-08 16:36:05', '2021-06-08 16:36:05', 'admin', 'admin', 0, '超级管理员', 0);
INSERT INTO `sys_role` VALUES (2, 0, '2022-09-09 01:46:59', '2022-09-09 01:46:59', 'admin', 'admin', 1, '审计人员', 1);
COMMIT;

-- ----------------------------
-- Table structure for user_login_log
-- ----------------------------
DROP TABLE IF EXISTS `user_login_log`;
CREATE TABLE `user_login_log` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `user_name` varchar(32) NOT NULL DEFAULT '' COMMENT '用户名',
  `login_ip` varchar(64) NOT NULL DEFAULT '' COMMENT '登录ip',
  `result` varchar(128) NOT NULL DEFAULT '' COMMENT '登录结果【  成功:T, 失败: F_密码不正确】',
  `gmt_modified` datetime DEFAULT NULL COMMENT '更新时间',
  `session_id` varchar(64) NOT NULL COMMENT '用户登录用的sessionid',
  `description` varchar(128) DEFAULT NULL COMMENT '备注',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `index_user_name` (`user_name`) USING BTREE,
  KEY `index_ip` (`login_ip`) USING BTREE,
  KEY `index_session_id` (`session_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=189 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户登录登出记录';

-- ----------------------------
-- Table structure for user_login_statistics
-- ----------------------------
DROP TABLE IF EXISTS `user_login_statistics`;
CREATE TABLE `user_login_statistics` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint NOT NULL DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '创建时间',
  `gmt_modified` datetime DEFAULT NULL COMMENT '更新时间',
  `user_name` varchar(32) NOT NULL DEFAULT '' COMMENT '用户名',
  `password_error_count` int NOT NULL DEFAULT '0' COMMENT '密码错误次数',
  `description` varchar(128) DEFAULT NULL COMMENT '备注',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `index_user_name` (`user_name`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户登录登出记录';

-- ----------------------------
-- Records of user_login_statistics
-- ----------------------------
BEGIN;
INSERT INTO `user_login_statistics` VALUES (7, 0, '2022-09-15 09:13:18', '2022-09-15 17:13:23', 'admin', 0, NULL);
COMMIT;

-- ----------------------------
-- Table structure for user_portrait_by_table
-- ----------------------------
DROP TABLE IF EXISTS `user_portrait_by_table`;
CREATE TABLE `user_portrait_by_table` (
  `id` int NOT NULL AUTO_INCREMENT,
  `username` varchar(256) COLLATE utf8mb4_bin NOT NULL,
  `table_name` varchar(256) COLLATE utf8mb4_bin NOT NULL COMMENT '库表名称',
  `count` int DEFAULT '0' COMMENT '访问次数',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `update_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `is_deleted` int NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_portrait_by_table_id_uindex` (`id`),
  KEY `user_portrait_by_table_create_time_index` (`create_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='基于访问库表的用户画像表';

-- ----------------------------
-- Table structure for user_portrait_by_time
-- ----------------------------
DROP TABLE IF EXISTS `user_portrait_by_time`;
CREATE TABLE `user_portrait_by_time` (
  `id` int NOT NULL AUTO_INCREMENT,
  `username` varchar(256) COLLATE utf8mb4_bin NOT NULL,
  `morning_rate` double DEFAULT '0' COMMENT '上午访问频率(5-13)',
  `afternoon_rate` double DEFAULT '0' COMMENT '下午访问频率(13-21)',
  `night_rate` double DEFAULT '0' COMMENT '夜间访问频率(21-5)',
  `create_time` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `update_time` timestamp NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `is_deleted` int NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`),
  UNIQUE KEY `user_portrait_by_time_id_uindex` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='基于访问时间的用户画像表';

-- ----------------------------
-- Table structure for user_portrait_by_visited_table
-- ----------------------------
DROP TABLE IF EXISTS `user_portrait_by_visited_table`;
CREATE TABLE `user_portrait_by_visited_table` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `rule_name` varchar(128) DEFAULT NULL COMMENT '规则名称',
  `visited_table` varchar(64) DEFAULT NULL COMMENT '访问过的表',
  `visited_count` int DEFAULT NULL COMMENT '访问的次数',
  `visited_db_instance` varchar(256) DEFAULT NULL COMMENT '表属于的数据库',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=46 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='基于访问过的数据表的用户画像表';

-- ----------------------------
-- Table structure for user_portrait_by_visited_table_everyday
-- ----------------------------
DROP TABLE IF EXISTS `user_portrait_by_visited_table_everyday`;
CREATE TABLE `user_portrait_by_visited_table_everyday` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `rule_name` varchar(128) DEFAULT NULL COMMENT '规则名称',
  `visited_table` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '访问过的表',
  `visited_count` int DEFAULT NULL COMMENT '访问的次数',
  `visited_date` datetime DEFAULT NULL COMMENT '访问表的日期',
  `db_type` varchar(16) DEFAULT NULL COMMENT '操作数据库表的类型：select、update、insert、delete',
  `visited_db_instance` varchar(256) DEFAULT NULL COMMENT '访问表所属的数据库',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk` (`user_name`,`visited_table`,`visited_date`,`db_type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='基于每天访问过的数据表的用户画像表';

-- ----------------------------
-- Table structure for user_portrait_by_visited_time
-- ----------------------------
DROP TABLE IF EXISTS `user_portrait_by_visited_time`;
CREATE TABLE `user_portrait_by_visited_time` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `rule_id` int DEFAULT NULL COMMENT '命中的规则',
  `rule_name` varchar(128) DEFAULT NULL COMMENT '规则名称',
  `forenoon_count` int DEFAULT NULL COMMENT '上午访问次数',
  `night_count` int DEFAULT NULL COMMENT '晚上访问次数',
  `afternoon_count` int DEFAULT NULL COMMENT '下午访问次数',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_user_name` (`user_name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='基于访问时间的用户画像表';

-- ----------------------------
-- Table structure for user_portrait_rules
-- ----------------------------
DROP TABLE IF EXISTS `user_portrait_rules`;
CREATE TABLE `user_portrait_rules` (
  `id` int unsigned NOT NULL AUTO_INCREMENT COMMENT '记录ID',
  `is_delete` tinyint DEFAULT '0' COMMENT '逻辑删除标志；0-未删除；1-已删除；',
  `gmt_create` datetime NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '开始处理请求的时间',
  `gmt_modified` datetime DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT '处理完请求的时间',
  `rule_name` varchar(128) DEFAULT NULL COMMENT '规则名称',
  `rule_desc` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '规则简洁描述',
  `rule_desc_detail` varchar(128) DEFAULT NULL COMMENT '规则详细描述',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户画像规则表';

-- ----------------------------
-- Records of user_portrait_rules
-- ----------------------------
BEGIN;
INSERT INTO `user_portrait_rules` VALUES (1, 0, '2022-07-27 06:39:07', '2022-07-27 06:39:07', 'user_visited_time', '基于访问时间段的异常告警', '基于访问时间段的异常告警规则：如果某用户通常白天访问数据，那么夜间访问数据则为异常情况，给出告警；');
INSERT INTO `user_portrait_rules` VALUES (2, 0, '2022-07-27 06:39:11', '2022-07-27 06:39:11', 'user_visited_table', '基于访问数据库表的异常告警', '基于访问数据库表的异常告警规则：某用户首次访问某个数据库表时，给出告警；');
COMMIT;

SET FOREIGN_KEY_CHECKS = 1;
