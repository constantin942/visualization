package com.mingshi.web.controller;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTableTask;
import com.mingshi.skyflying.anomaly_detection.task.UserPortraitByTimeTask;
import com.mingshi.skyflying.aspect.OperationAuditAspectAnnotation;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.service.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.text.ParseException;
import java.util.List;
import java.util.Map;

/**
 * @Author zhaoming
 * @Description 不需要登录校验的接口写这里面
 * @Date 15:28 2020/2/2
 * @Param
 * @return
 **/
@Controller
@Slf4j
@Validated
@RequestMapping("/api/skyflying")
public class SkyflyingController {

  @Resource
  private MsMonitorBusinessSystemTablesService msMonitorBusinessSystemTablesService;
  @Resource
  private MsAgentSwitchService msAgentSwitchService;
  @Resource
  private SegmentDetailService segmentDetailService;
  @Resource
  private AuditLogService auditLogService;
  @Resource
  private MsAlarmInformationService msAlarmInformationService;
//  @Resource
//  private UserPortraitByVisitedTimeService userPortraitByTimeService;
  @Resource
  private UserPortraitByVisitedTableService userPortraitByTableService;
  @Resource
  private UserPortraitRulesService userPortraitRulesService;
  @Resource
  private MsAgentInformationService msAgentInformationService;
  @Resource
  private SysMenuService sysMenuService;
  @Resource
  private OperationLogService operationLogService;
  @Resource
  private UserPortraitByTimeTask timeTask;

  /**
   * <B>方法名称：getHighDangerOperationLog</B>
   * <B>概要说明：获取高危操作日志</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年09月09日 15:09:50
   * @Param [userName]
   **/
  @ResponseBody
  @GetMapping(value = "/getHighDangerOperationLog")
  public ServerResponse<String> getHighDangerOperationLog(@RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                          @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize,
                                                          String userName) {
    return operationLogService.getOperationLog(userName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getSysMenu</B>
   * <B>概要说明：获取菜单</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.Object>
   * @Author zm
   * @Date 2022年09月09日 09:09:54
   * @Param [userName]
   **/
  @ResponseBody
  @PostMapping(value = "/sysmenu")
  public ServerResponse<String> getSysMenu(String userName) {
    return sysMenuService.getSysMenu(userName);
  }

  /**
   * <B>方法名称：allAgentOperationRecord</B>
   * <B>概要说明：获取指定探针的操作记录</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年08月25日 16:08:13
   * @Param [serviceInstance, agentSwitch]
   **/
  @ResponseBody
  @GetMapping(value = "/allAgentOperationRecord")
  public ServerResponse<String> allAgentOperationRecord(@RequestParam(value = "serviceInstance") String serviceInstance,
                                                        @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                        @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAgentSwitchService.allAgentOperationRecord(serviceInstance.trim(), pageNo, pageSize);
  }

  /**
   * <B>方法名称：queryAgentStatus</B>
   * <B>概要说明：查询探针状态</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年08月26日 10:08:20
   * @Param [serviceInstance]
   **/
  @ResponseBody
  @GetMapping(value = "/queryAgentStatus")
  public ServerResponse<String> queryAgentStatus(@RequestParam(value = "serviceInstance") String serviceInstance) {
    return msAgentSwitchService.queryAgentStatus(serviceInstance.trim());
  }

  /**
   * <B>方法名称：updateAgentStatus</B>
   * <B>概要说明：更新探针的状态：开关</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年08月25日 10:08:53
   * @Param [pageNo, pageSize]
   **/
  @OperationAuditAspectAnnotation(isStart = true)
  @ResponseBody
  @PostMapping(value = "/updateAgentStatus")
  public ServerResponse<String> updateAgentStatus(@RequestParam(value = "serviceInstance") String serviceInstance, @RequestParam(value = "agentSwitch") String agentSwitch) {
    return msAgentSwitchService.updateAgentStatus(serviceInstance.trim(), agentSwitch.trim());
  }

  /**
   * <B>方法名称：getAllMonitorTables</B>
   * <B>概要说明：获取所有监管的表</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月13日 14:07:42
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getAllMonitorTables")
  public ServerResponse<String> getAllMonitorTables(
    String tableName,
    String dbName,
    String dbAddress,
    String tableDesc,
    @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
    @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msMonitorBusinessSystemTablesService.getAllTables(tableDesc, tableName, dbName, dbAddress, pageNo, pageSize);
  }

  @OperationAuditAspectAnnotation(isStart = true)
  @ResponseBody
  @PostMapping(value = "/updateMonitorTableDesc")
  public ServerResponse<String> updateMonitorTableDesc(@RequestParam(value = "id") Integer id, @RequestParam(value = "tableDesc") String tableDesc, String tableName) {
    return msMonitorBusinessSystemTablesService.updateTableDesc(id, tableDesc);
  }

  /**
   * <B>方法名称：updateMonitorTable</B>
   * <B>概要说明：更新监管的表状态</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月13日 14:07:42
   * @Param []
   **/
  @OperationAuditAspectAnnotation(isStart = true)
  @ResponseBody
  @PostMapping(value = "/updateMonitorTable")
  public ServerResponse<String> updateMonitorTable(@RequestParam(value = "id") Integer id,
                                                   @RequestParam(value = "isDelete") Integer isDelete, String tableName) {
    return msMonitorBusinessSystemTablesService.updateTableInformation(id, isDelete);
  }

  /**
   * <B>方法名称：updateSkywalkingAgent</B>
   * <B>概要说明：更新探针别名</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月29日 14:06:30
   * @Param [agentCode, pageNo, pageSize]
   **/
  @OperationAuditAspectAnnotation(isStart = true)
  @ResponseBody
  @GetMapping(value = "/updateSkywalkingAgent")
  public ServerResponse<String> updateSkywalkingAgent(
    @RequestParam(value = "id") Integer id,
    @RequestParam(value = "agentName") String agentName,
    String agentCode) {
    return msAgentInformationService.updateSkywalkingAgent(id, agentName);
  }

  /**
   * <B>方法名称：getAllSkywalkingAgent</B>
   * <B>概要说明：探针管理---从数据库中获取所有的探针信息</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月29日 10:06:11
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getAllSkywalkingAgent")
  public ServerResponse<String> getAllSkywalkingAgent(
    String agentCode,
    @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
    @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAgentInformationService.getAllSkywalkingAgent(agentCode, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getActiveSkywalkingAgent</B>
   * <B>概要说明：获取存活的探针</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月27日 14:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getActiveSkywalkingAgent")
  public ServerResponse<String> getActiveSkywalkingAgent() {
    return msAgentInformationService.getActiveSkywalkingAgent();
  }

  /**
   * <B>方法名称：getUserPortraitRules</B>
   * <B>概要说明：获取所有的规则</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月23日 15:06:53
   * @Param [pageNo, pageSize]
   **/
  @ResponseBody
  @GetMapping(value = "/getUserPortraitRules")
  public ServerResponse<String> getUserPortraitRules(@RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                     @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return userPortraitRulesService.getAllUserPortraitRules(pageNo, pageSize);
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTimeRule</B>
   * <B>概要说明：禁启用用户在什么时间访问了多少次系统规则</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 17:06:55
   * @Param []
   **/
  @ResponseBody
  @PostMapping(value = "/updateUserPortraitRule")
  public ServerResponse<String> updateUserPortraitRule(@RequestParam(value = "ruleId") Integer ruleId, @RequestParam(value = "isDelete") Integer isDelete) {
    return userPortraitRulesService.updateUserPortraitRule(ruleId, isDelete);
  }

  /**
   * <B>方法名称：getAllTableNameFromDMS</B>
   * <B>概要说明：获取所有数据库表名</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 17:06:51
   * @Param [dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize]
   **/
  @ResponseBody
  @GetMapping(value = "/getAllTableNameFromDMS")
  public ServerResponse<String> getAllTableNameFromDms() {
    return auditLogService.getAllTableNameFromDms();
  }

  /**
   * <B>方法名称：getAllSqlTypeFromDMS</B>
   * <B>概要说明：获取所有sql语句的操作类型</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 17:06:51
   * @Param [dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize]
   **/
  @ResponseBody
  @GetMapping(value = "/getAllSqlTypeFromDMS")
  public ServerResponse<String> getAllSqlTypeFromDms() {
    return auditLogService.getAllSqlTypeFromDms();
  }

  /**
   * <B>方法名称：getAllUserNameFromDMS</B>
   * <B>概要说明：获取所有操作DMS的用户名</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 17:06:51
   * @Param [dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize]
   **/
  @ResponseBody
  @GetMapping(value = "/getAllUserNameFromDMS")
  public ServerResponse<String> getAllUserNameFromDms() {
    return auditLogService.getAllUserNameFromDms();
  }

  /**
   * <B>方法名称：getDmsAuditLogFromDb</B>
   * <B>概要说明：从数据库中获取DMS的数据库审计日志</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 15:06:08
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getDmsAuditLogFromDb")
  public ServerResponse<String> getDmsAuditLogFromDb(String dbUserName, /* 访问数据库的用户名 */
                                                     String sqlType, /* SQL语句的类型；是insert、select、update、delete等 */
                                                     String msTableName, /* 数据库表名 */
                                                     String startTime, /* 开始时间 */
                                                     String endTime, /* 结束时间 */
                                                     @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                     @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return auditLogService.getDmsAuditLogFromDb(dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getUserNameAnomalyDetectionInfo</B>
   * <B>概要说明：获取异常信息中的所有的用户名</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getUserNameAnomalyDetectionInfo")
  public ServerResponse<String> getUserNameAnomalyDetectionInfo() {
    return msAlarmInformationService.getUserNameAnomalyDetectionInfo();
  }


  /**
   * <B>方法名称：getAllAlarmInfoDetailByUserName</B>
   * <B>概要说明：获取指定用户所有的异常信息</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月24日 16:06:19
   * @Param [userName, pageNo, pageSize]
   **/
  @ResponseBody
  @GetMapping(value = "/getAllAlarmInfoDetailByUserName")
  public ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName,
                                                                Integer matchRuleId,
                                                                String originalTime,
                                                                @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAlarmInformationService.getAllAlarmInfoDetailByUserName(userName, matchRuleId, originalTime, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAnomalyDetectionInfo</B>
   * <B>概要说明：获取异常信息</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getAnomalyDetectionInfo")
  public ServerResponse<String> getAnomalyDetectionInfo(String userName,
                                                        @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                        @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAlarmInformationService.getAllAlarmInfo(userName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAnomalyDetectionInfoByGroupByUserName</B>
   * <B>概要说明：根据用户名分组获取告警摘要信息</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月30日 09:06:06
   * @Param [pageNo, pageSize]
   **/
  @ResponseBody
  @GetMapping(value = "/getAnomalyDetectionInfoByGroupByUserName")
  public ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(
    @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
    @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAlarmInformationService.getAnomalyDetectionInfoByGroupByUserName(pageNo, pageSize);
  }

  /**
   * <B>方法名称：updateAnomalyDetectionInfo</B>
   * <B>概要说明：删除告警信息或更新画像</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月25日 13:07:16
   * @Param [pageNo, pageSize]
   **/
  @ResponseBody
  @PostMapping(value = "/updateAnomalyDetectionInfo")
  public ServerResponse updateAnomalyDetectionInfo(@Valid @RequestBody List<AnomalyDetectionInfoBo> anomalyDetectionInfoBos) {
    msAlarmInformationService.updateAnomalyDetectionInfos(anomalyDetectionInfoBos);
    return ServerResponse.createBySuccess();
  }

//  /**
//   * <B>方法名称：getAllUserNamePortraitByVisitedTime</B>
//   * <B>概要说明：获取所有的用户</B>
//   *
//   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
//   * @Author zm
//   * @Date 2022年06月13日 09:06:57
//   * @Param []
//   **/
//  @ResponseBody
//  @GetMapping(value = "/getAllUserNamePortraitByVisitedTime")
//  public ServerResponse<String> getAllUserNamePortraitByVisitedTime() {
//    return userPortraitByTimeService.getAllUserNamePortraitByVisitedTime();
//  }

//  /**
//   * <B>方法名称：getUserPortraitByVisitedTime</B>
//   * <B>概要说明：获取用户在什么时间访问了多少次系统</B>
//   *
//   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
//   * @Author zm
//   * @Date 2022年06月13日 09:06:57
//   * @Param []
//   **/
//  @ResponseBody
//  @GetMapping(value = "/getAllUserPortraitByVisitedTime")
//  public ServerResponse<String> getAllUserPortraitByVisitedTime(String userName, /* 登录系统的名称 */
//                                                                @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
//                                                                @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
//    return userPortraitByTimeService.getAllUserPortraitByVisitedTime(userName, pageNo, pageSize);
//  }

  /**
   * <B>方法名称：getAllVisitedTablePortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取所有的表名</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getAllVisitedTablePortraitByVisitedTableEveryday")
  public ServerResponse<String> getAllVisitedTablePortraitByVisitedTableEveryday() {
    return userPortraitByTableService.getAllVisitedTablePortraitByVisitedTableEveryday();
  }

  /**
   * <B>方法名称：getUserPortraitByVisitedTime</B>
   * <B>概要说明：获取用户在什么时间访问了多少次系统</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getUserPortraitByVisitedTime")
  public ServerResponse<String> getUserPortraitByVisitedTime() {
    return userPortraitByTableService.getAllVisitedTablePortraitByVisitedTableEveryday();
  }

//  /**
//   * <B>方法名称：updateUserPortraitByVisitedTimeRule</B>
//   * <B>概要说明：禁启用用户在什么时间访问了多少次系统规则</B>
//   *
//   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
//   * @Author zm
//   * @Date 2022年06月16日 17:06:55
//   * @Param []
//   **/
//  @ResponseBody
//  @GetMapping(value = "/updateUserPortraitByVisitedTimeRule")
//  public ServerResponse<String> updateUserPortraitByVisitedTimeRule(@RequestParam(value = "ruleId") Integer ruleId, @RequestParam(value = "isDelete") Integer isDelete) {
//    return userPortraitByTimeService.updateUserPortraitByVisitedTimeRule(ruleId, isDelete);
//  }

  /**
   * <B>方法名称：getAllUserNameUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取所有的用户名</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getAllUserNameUserPortraitByVisitedTableEveryday")
  public ServerResponse<String> getAllUserNameUserPortraitByVisitedTableEveryday() {
    return userPortraitByTableService.getAllUserNameUserPortraitByVisitedTableEveryday();
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：更新用户访问过的表的画像规则：isDelete=0，启用这个规则；isDelete=1，禁用这个规则</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 14:06:35
   * @Param [ruleId, isDelete]
   **/
  @ResponseBody
  @GetMapping(value = "/updateUserPortraitByVisitedTableEverydayRule")
  public ServerResponse updateUserPortraitByVisitedTableEverydayRule(@RequestParam(value = "ruleId") Integer ruleId, @RequestParam(value = "isDelete") Integer isDelete) {
    userPortraitRulesService.updateRule(ruleId, isDelete);
    return ServerResponse.createBySuccess();
  }

//  /**
//   * <B>方法名称：addUserPortraitByVisitedTtimeRule</B>
//   * <B>概要说明：增加用户在什么时间访问过系统多少次的画像规则</B>
//   *
//   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
//   * @Author zm
//   * @Date 2022年06月16日 17:41:35
//   * @Param [ruleId, isDelete]
//   **/
//  @ResponseBody
//  @GetMapping(value = "/addUserPortraitByVisitedTtimeRule")
//  public ServerResponse<String> addUserPortraitByVisitedTtimeRule(@RequestParam(value = "userName") String userName,
//                                                                  @RequestParam(value = "forenoonCount") Integer forenoonCount,
//                                                                  @RequestParam(value = "afternoonCount") Integer afternoonCount,
//                                                                  @RequestParam(value = "nightCount") Integer nightCount) {
//    return userPortraitByTimeService.addUserPortraitByVisitedTtimeRule(userName, forenoonCount, afternoonCount, nightCount);
//  }

  /**
   * <B>方法名称：addUserPortraitByVisitedTableEverydayRule</B>
   * <B>概要说明：增加用户访问过的表的画像规则</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 16:41:35
   * @Param [ruleId, isDelete]
   **/
  @ResponseBody
  @GetMapping(value = "/addUserPortraitByVisitedTableEverydayRule")
  public ServerResponse<String> addUserPortraitByVisitedTableEverydayRule(@RequestParam(value = "userName") String userName,
                                                                          @RequestParam(value = "visitedTable") String visitedTable,
                                                                          @RequestParam(value = "visitedCount") Integer visitedCount,
                                                                          @RequestParam(value = "visitedDate") String visitedDate,
                                                                          @RequestParam(value = "dbType") String dbType) {
    return userPortraitByTableService.addUserPortraitByVisitedTableEverydayRule(userName, visitedTable, visitedCount, visitedDate, dbType);
  }

  /**
   * <B>方法名称：getUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取用户每天访问表的次数的画像信息</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/getUserPortraitByVisitedTableEveryday")
  public ServerResponse<String> getUserPortraitByVisitedTableEveryday(String userName, /* 登录系统的名称 */
                                                                      String visitedTable, /* 访问数据库的表 */
                                                                      String visitedDate, /* 访问数据库表的时间 */
                                                                      String visitedDbInstance,/*访问的数据库*/
                                                                      @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                      @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {


    return userPortraitByTableService.getUserPortraitByVisitedTableEveryday(userName, visitedTable, visitedDate, visitedDbInstance, pageNo, pageSize);
  }


  /**
   * <B>方法名称：userPortraitByVisitedTable</B>
   * <B>概要说明：基于历史数据，统计用户每天访问表的次数，以此来生成用户的画像</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月08日 14:06:08
   * @Param []
   **/
  @ResponseBody
  @GetMapping(value = "/userPortraitByVisitedTableEveryday")
  public ServerResponse<String> userPortraitByVisitedTableEveryday() {
    return userPortraitByTableService.createUserPortraitByVisitedTableEveryday();
  }

//  /**
//   * <B>方法名称：userPortraitByVisitedTime</B>
//   * <B>概要说明：基于历史数据，统计用户访问系统的时间，以此来生成用户的画像</B>
//   *
//   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
//   * @Author zm
//   * @Date 2022年06月07日 14:06:08
//   * @Param []
//   **/
//  @ResponseBody
//  @GetMapping(value = "/userPortraitByVisitedTime")
//  public ServerResponse<String> userPortraitByVisitedTime() {
//    return userPortraitByTimeService.createUserPortraitByVisitedTime();
//  }

  /**
   * <B>方法名称：getAllUserName/B>
   * <B>概要说明：获取所有的用户名</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年06月1日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getAllUserName")
  public ServerResponse<String> getAllUserName() {
    return segmentDetailService.getAllUserName();
  }



  /**
   * <B>方法名称：getAllMsTableName/B>
   * <B>概要说明：获取所有的库表名</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年06月1日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getAllMsTableName")
  public ServerResponse<String> getAllMsTableName() {
    return segmentDetailService.getAllMsTableName();
  }


  /**
   * <B>方法名称：getAllInstanceAndTableName/B>
   * <B>概要说明：获取所有的库表名以及数据库名</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年06月1日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getAllInstanceAndTableName")
  public ServerResponse<InstanceTable> getAllInstanceAndTableName() {
    return segmentDetailService.getAllInstanceAndTableName();
  }

  /**
   * <B>方法名称：getCoarseCountsOfUser/B>
   * <B>概要说明：获取用户对数据的粗粒度信息。信息概况 -> 用户访问行为</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/

  @ResponseBody
  @GetMapping(value = "/getCoarseCountsOfUser")
  public ServerResponse<String> getCoarseCountsOfUser(@RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                      @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return segmentDetailService.getCoarseCountsOfUser(pageNo, pageSize);
  }

  /**
   * <B>方法名称：getCoarseCountsOfTableName/B>
   * <B>概要说明：获取数据库的粗粒度信息；信息概况 -> 数据访问行为</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   * @Question：计算数据过慢
   **/
  @ResponseBody
  @GetMapping(value = "/getCoarseCountsOfTableName")
  public ServerResponse<String> getCoarseCountsOfTableName(
    String tableName,
    @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
    @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return segmentDetailService.getCoarseCountsOfTableName(tableName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getCoarseCountsOfOneUser/B>
   * <B>概要说明：获取某一特定用户对数据的粗粒度信息</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getCoarseCountsOfOneUser")
  public ServerResponse<UserCoarseInfo> getCoarseCountsOfOneUser(@RequestParam(value = "applicationUserName", defaultValue = "") String applicationUserName, /* 登录系统的名称 */
                                                                 @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                 @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {

    return segmentDetailService.getCoarseCountsOfOneUser(applicationUserName, pageNo, pageSize);

  }


  /**
   * <B>方法名称：getCountsOfUser/B>
   * <B>概要说明：获取用户对数据的详细数据</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getCountsOfUser")
  public ServerResponse<List<String>> getCountsOfUser(@RequestParam(value = "tableName") String tableName /* 数据库表名 */) {
    return segmentDetailService.getCountsOfUser(tableName);
  }

  /**
   * <B>方法名称：getUserOperationTypeCount</B>
   * <B>概要说明：获取用户操作类型次数</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.util.List < java.lang.String>>
   * @Author zm
   * @Date 2022年07月22日 17:07:46
   * @Param [userName]
   **/
  @ResponseBody
  @GetMapping(value = "/getUserOperationTypeCount")
  public ServerResponse<List<String>> getUserOperationTypeCount(@RequestParam(value = "userName") String userName) {
    return segmentDetailService.getUserOperationTypeCount(userName);
  }

  /**
   * <B>方法名称：getCountsOfUserRecentSevenDays/B>
   * <B>概要说明：数据分布 --> 详情 --> 近七天数据访问统计；获取用户对数据的详细数据</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getCountsOfUserRecentSevenDays")
  public ServerResponse<List<Long>> getCountsOfUserRecentSevenDays(@RequestParam(value = "tableName") String tableName, /* 数据库表名 */
                                                                   @RequestParam(value = "startTime") String startTime, /* 开始时间 */
                                                                   @RequestParam(value = "endTime") String endTime, /* 结束时间 */
                                                                   @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                   @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) throws ParseException {
    return segmentDetailService.getCountsOfUserUserRecentSevenDays(tableName, startTime, endTime, pageNo, pageSize);
  }


  /**
   * <B>方法名称：getCountsOfAllRecentSevenDays/B>
   * <B>概要说明：获取"信息概况" -> "近7天信息采集情况"</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getCountsOfAllRecentSevenDays")
  public ServerResponse<List<Long>> getCountsOfAllRecentSevenDays(@RequestParam(value = "startTime") String startTime, /* 开始时间 */
                                                                  @RequestParam(value = "endTime") String endTime /* 结束时间 */) {
    return segmentDetailService.getCountsOfAllRecentSevenDays(startTime, endTime);
  }

  /**
   * <B>方法名称：getOverviewOfSystem/B>
   * <B>概要说明：获取可视化系统关于、管理信息数量、用户数量的总览信息。信息概况 -> 数据统计情况</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getOverviewOfSystem")
  public ServerResponse<SystemOverview> getOverviewOfSystem() {
    return segmentDetailService.getOverviewOfSystem();
  }


  /**
   * <B>方法名称：getUserUsualAndUnusualData/B>
   * <B>概要说明：获取用户的经常访问数据和不常访问数据</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月6日 14:30:19
   * @Param
   **/

  @ResponseBody
  @GetMapping(value = "/getUserUsualAndUnusualData")
  public ServerResponse<Map<String, List<UserUsualAndUnusualVisitedData>>> getUserUsualAndUnusualData(String applicationUserName) {
    return segmentDetailService.getUserUsualAndUnusualData(applicationUserName);
  }

  /**
   * <B>方法名称：getAlarmData/B>
   * <B>概要说明：获取告警数据</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月6日 14:30:19
   * @Param
   **/
  @ResponseBody
  @GetMapping(value = "/getAlarmData")
  public ServerResponse<List<AlarmData>> getAlarmData() {
    return segmentDetailService.getAlarmData();
  }

  /**
   * <B>方法名称：getAlarmData/B>
   * <B>概要说明：获取高危用户</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月6日 14:30:19
   * @Param
   **/

  @ResponseBody
  @GetMapping(value = "/getUserAlarmData")
  public ServerResponse<List<UserAlarmData>> getUserAlarmData() {
    return segmentDetailService.getUserAlarmData();
  }



  /**
   * <B>方法名称：getAllSegments</B>
   * <B>概要说明：获取所有的访问链条信息，版本2的实现</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author zm
   * @Date 2022年06月02日 17:15:19
   * @Param [request, userName, password]
   **/
  @ResponseBody
  @GetMapping(value = "/getAllSegments")
  public ServerResponse<String> getAllSegments(String applicationUserName, /* 登录系统的名称 */
                                               String dbUserName, /* 访问数据库的用户名 */
                                               String dbType, /* SQL语句的类型；是insert、select、update、delete等 */
                                               String msTableName, /* 数据库表名 */
                                               String startTime, /* 开始时间 */
                                               String endTime, /* 结束时间 */
                                               String operationType, /* 操作行为 */
                                               @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                               @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return segmentDetailService.getAllSegmentsBySegmentRelation(applicationUserName, dbType, msTableName, startTime, endTime, dbUserName, operationType, pageNo, pageSize);
  }

  /**
   * <B>方法名称：</B>
   * <B>概要说明：</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 16:05:50
   * @Param [startTime, endTime]
   **/
  @ResponseBody
  @GetMapping(value = "/autoFetchAuditlogByDMS")
  public ServerResponse<String> autoFetchAuditlogByDms(@RequestParam("startTime") String startTime, @RequestParam("endTime") String endTime) {
    return auditLogService.autoFetchAuditlogByDms(startTime, endTime);
  }


  @ResponseBody
  @GetMapping(value = "/getVisitRate")
  public ServerResponse<Map<String, Double>> getVisitRate(@RequestParam("username") String username) {
    return ServerResponse.createBySuccess(timeTask.getVisitRate(username));
  }
}
