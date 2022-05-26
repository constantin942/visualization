package com.mingshi.skyflying.controller;

import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.annotation.Resource;
import java.util.List;

/**
 * @Author zhaoming
 * @Description 不需要登录校验的接口写这里面
 * @Date 15:28 2020/2/2
 * @Param
 * @return
 **/
@Controller
@Slf4j
@RequestMapping("/api/skyflying")
public class SkyflyingController {

  @Resource
  private SegmentDetailService segmentDetailService;
  @Resource
  private AuditLogService auditLogService;

  /**
   * <B>方法名称：getAllSegments</B>
   * <B>概要说明：获取所有的访问链条信息</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author zm
   * @Date 2022年04月19日 17:04:19
   * @Param [request, userName, password]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllSegments", method = RequestMethod.GET)
  public ServerResponse<String> getAllSegments(String userName, Integer pageNo, Integer pageSize) {
    return segmentDetailService.getAllSegmentsBySegmentRelation(userName, pageNo, pageSize);
  }
  /**
   * <B>方法名称：</B>
   * <B>概要说明：</B>
   * @Author zm
   * @Date 2022年05月26日 16:05:50
   * @Param [startTime, endTime]
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   **/
  @ResponseBody
  @RequestMapping(value = "/autoFetchAuditlogByDMS", method = RequestMethod.GET)
  public ServerResponse<String> autoFetchAuditlogByDMS(@RequestParam("startTime") String startTime, @RequestParam("endTime") String endTime) {
    return auditLogService.autoFetchAuditlogByDMS(startTime, endTime);
  }

  @ResponseBody
  @RequestMapping(value = "/processAuditLog", method = RequestMethod.GET)
  public ServerResponse<String> processAuditLog() {
    String auditLog = "{\n" +
      "  \"TotalCount\": 28,\n" +
      "  \"RequestId\": \"F4C0AA93-9088-54D7-A46D-D42C921160EF\",\n" +
      "  \"Success\": true,\n" +
      "  \"SQLExecAuditLogList\": {\n" +
      "    \"SQLExecAuditLog\": [\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-07 12:43:01\",\n" +
      "        \"SQL\": \"show variables like '%timeout%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 28\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 24,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-07 12:40:49\",\n" +
      "        \"SQL\": \"show variables like '%max_connection_errors%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-07 12:39:56\",\n" +
      "        \"SQL\": \"show variables like '%timeout%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 28\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 24,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-07 12:39:49\",\n" +
      "        \"SQL\": \"show variables like '%timeout%'；\",\n" +
      "        \"remark\": \"You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near '；' at line 1\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:36:47\",\n" +
      "        \"SQL\": \"show variables like '%general%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 2\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 24,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:36:27\",\n" +
      "        \"SQL\": \"show variables like 'log_bin'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 1\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-06 17:36:03\",\n" +
      "        \"SQL\": \"select host,user,account_locked from mysql.user\",\n" +
      "        \"remark\": \"Unknown column 'account_locked' in 'field list'\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:35:31\",\n" +
      "        \"SQL\": \"select host,user,select_priv,insert_priv,update_priv from mysql.user\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 23\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:34:45\",\n" +
      "        \"SQL\": \"show variables like '%ssl%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 9\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 24,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:33:53\",\n" +
      "        \"SQL\": \"select host,user from mysql.user\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 23\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:33:40\",\n" +
      "        \"SQL\": \"show variables like '%max_connection_errors%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:32:12\",\n" +
      "        \"SQL\": \"show variables like '%timeout%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 28\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-06 17:30:56\",\n" +
      "        \"SQL\": \"show variables like\\\"%timeout%\\\"；\",\n" +
      "        \"remark\": \"You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near '；' at line 1\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 34,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-06 17:30:28\",\n" +
      "        \"SQL\": \"show variables like“%timeout%”；\",\n" +
      "        \"remark\": \"You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near 'like“%timeout%”；' at line 1\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:30:00\",\n" +
      "        \"SQL\": \"show variables like '%max_connection_errors%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:29:29\",\n" +
      "        \"SQL\": \"show variables like 'default_password_lifetime%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:29:06\",\n" +
      "        \"SQL\": \"show variables like 'validate_password%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-06 17:27:56\",\n" +
      "        \"SQL\": \"select user,password_last_changed from mysql.user\",\n" +
      "        \"remark\": \"Unknown column 'password_last_changed' in 'field list'\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-06 17:27:51\",\n" +
      "        \"SQL\": \"select user,password_last_changed from mysql.user\",\n" +
      "        \"remark\": \"Unknown column 'password_last_changed' in 'field list'\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 24,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:27:39\",\n" +
      "        \"SQL\": \"show variables like 'default_password_lifetime%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 27,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:26:50\",\n" +
      "        \"SQL\": \"show variables like 'validate_password%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 23,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:26:42\",\n" +
      "        \"SQL\": \"show variables like 'validate_password%'\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SHOW\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 22,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:24:20\",\n" +
      "        \"SQL\": \"select @@version\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 1\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 24,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-05-06 17:24:09\",\n" +
      "        \"SQL\": \"select @@version.\",\n" +
      "        \"remark\": \"You have an error in your SQL syntax; check the manual that corresponds to your MySQL server version for the right syntax to use near '' at line 1\",\n" +
      "        \"instanceName\": \"mysql@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333033,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"mysql\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 21,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:11:48\",\n" +
      "        \"SQL\": \"select avg(convenient) as 便捷程度 , avg(response) as 响应速度,avg(quality) as 服务质量 from hy_evaluate\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"hy2_company_pro@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333029,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"hy2_company_pro\",\n" +
      "        \"affectRows\": 1\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1657729,\n" +
      "        \"elapsedTime\": 30,\n" +
      "        \"execState\": \"SUCCESS\",\n" +
      "        \"opTime\": \"2022-05-06 17:11:40\",\n" +
      "        \"SQL\": \"select count(1) from hy_user where company_type=1\",\n" +
      "        \"remark\": \"\",\n" +
      "        \"instanceName\": \"hy2_company_pro@pc-bp1l926n8kph3u2u4.mysql.polardb.rds.aliyuncs.com:3306【渔省心】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14333029,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SELECT\",\n" +
      "        \"schemaName\": \"hy2_company_pro\",\n" +
      "        \"affectRows\": 1\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1411652,\n" +
      "        \"elapsedTime\": 0,\n" +
      "        \"execState\": \"NOEXE\",\n" +
      "        \"opTime\": \"2022-01-07 15:05:08\",\n" +
      "        \"SQL\": \"set global wait_timeout= 200\",\n" +
      "        \"remark\": \"Access denied for user 'dev'@'%' to database 'sys'\",\n" +
      "        \"instanceName\": \"sys@rm-2ze2g566d6uwoa273.mysql.rds.aliyuncs.com:3505【开发测试数据库】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 22345555,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SET\",\n" +
      "        \"schemaName\": \"sys\",\n" +
      "        \"affectRows\": 0\n" +
      "      },\n" +
      "      {\n" +
      "        \"userName\": \"chgk\",\n" +
      "        \"instanceId\": 1411652,\n" +
      "        \"elapsedTime\": 38,\n" +
      "        \"execState\": \"FAIL\",\n" +
      "        \"opTime\": \"2022-01-07 15:04:49\",\n" +
      "        \"SQL\": \"set global wait_timeout=200\",\n" +
      "        \"remark\": \"Access denied; you need (at least one of) the SUPER privilege(s) for this operation\",\n" +
      "        \"instanceName\": \"xxl_job@rm-2ze2g566d6uwoa273.mysql.rds.aliyuncs.com:3505【开发测试数据库】\",\n" +
      "        \"userId\": 326614,\n" +
      "        \"dbId\": 14662999,\n" +
      "        \"logic\": 0,\n" +
      "        \"SQLType\": \"SET\",\n" +
      "        \"schemaName\": \"xxl_job\",\n" +
      "        \"affectRows\": 0\n" +
      "      }\n" +
      "    ]\n" +
      "  }\n" +
      "}";
    ListSQLExecAuditLogResponseBody body = JsonUtil.string2Obj(auditLog, ListSQLExecAuditLogResponseBody.class);
    ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogList sqlExecAuditLogList = body.getSQLExecAuditLogList();
    List<ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog> listSQLExecAuditLogList = sqlExecAuditLogList.SQLExecAuditLog;
    auditLogService.process(listSQLExecAuditLogList);
    return null;
  }


}
