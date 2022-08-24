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

 Date: 24/08/2022 14:17:25
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

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
) ENGINE=InnoDB AUTO_INCREMENT=57 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin COMMENT='探针信息表';

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
) ENGINE=InnoDB AUTO_INCREMENT=367 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='数据库审计日志表';

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
  `table_name` varchar(64) DEFAULT NULL COMMENT '表名',
  `db_name` varchar(128) DEFAULT NULL COMMENT '数据库名称',
  `db_address` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库地址',
  `table_desc` varchar(64) DEFAULT NULL COMMENT '表中存储什么类型的数据',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_table_name` (`table_name`,`db_name`,`db_address`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=972 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='监管业务系统数据库表';

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
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='定时拉取DMS审计日志任务表';

-- ----------------------------
-- Table structure for ms_segment_detail
-- ----------------------------
DROP TABLE IF EXISTS `ms_segment_detail`;
CREATE TABLE `ms_segment_detail` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
  `user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的用户名',
  `service_code` varchar(256) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当应用系统与探针一起启动时，操作人员设置的应用系统名称',
  `start_time` datetime DEFAULT NULL COMMENT '请求开始时间',
  `db_type` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '对数据库的操作类型，select：查询、insert：插入、update：更新、delete：删除',
  `db_user_name` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '数据库用户名',
  `operation_name` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '登录系统的URL',
  `token` varchar(512) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'token',
  `global_trace_id` varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '全局traceId',
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
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='用户操作记录详情表';

-- ----------------------------
-- Table structure for ms_segment_detail_username_is_null
-- ----------------------------
DROP TABLE IF EXISTS `ms_segment_detail_username_is_null`;
CREATE TABLE `ms_segment_detail_username_is_null` (
  `id` int NOT NULL AUTO_INCREMENT COMMENT '主键',
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

SET FOREIGN_KEY_CHECKS = 1;
