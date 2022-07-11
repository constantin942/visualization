package com.mingshi.skyflying.controller;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.anomaly_detection.singleton.StatisticsConsumeProcessorThreadQPS;
import com.mingshi.skyflying.domain.*;
import com.mingshi.skyflying.exception.AiitExceptionCode;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.*;
import com.mingshi.skyflying.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.jdbc.Null;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.annotation.Resource;
import java.text.ParseException;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

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
  @Resource
  private MsAlarmInformationService msAlarmInformationService;
  @Resource
  private UserPortraitByVisitedTimeService userPortraitByTimeService;
  @Resource
  private UserPortraitByVisitedTableService userPortraitByTableService;
  @Resource
  private MsThirdPartyTableListService msThirdPartyTableListService;
  @Resource
  private MsThirdPartyTableFieldsService msThirdPartyTableFieldsService;
  @Resource
  private UserPortraitRulesService userPortraitRulesService;
  @Resource
  private MsAgentInformationService msAgentInformationService;

  /**
   * <B>方法名称：getQps</B>
   * <B>概要说明：获取消费线程的QPS</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月29日 17:06:41
   * @Param [id, agentName]
   **/
  @ResponseBody
  @RequestMapping(value = "/getQps", method = RequestMethod.GET)
  public ServerResponse<String> getQps() {
    Map<String/* 时间 */, Integer> countMap1 = new HashMap<>();
    Map<String/* 线程名称 */, Map<String/* 时间 */, AtomicInteger/* 在当前时间内的处理消息的数量 */>> threadNameTimeCountMap =
      StatisticsConsumeProcessorThreadQPS.getStatisticsConsumeProcessorThreadQPSMap();

    Integer maxValue1 = -1;
    if (null != threadNameTimeCountMap && 0 < threadNameTimeCountMap.size()) {
      Iterator<String> iterator = threadNameTimeCountMap.keySet().iterator();
      while (iterator.hasNext()) {
        String threadName = iterator.next();
        Map<String/* 时间 */, AtomicInteger/* 在当前时间内的处理消息的数量 */> timeCountMap = threadNameTimeCountMap.get(threadName);
        if (null != timeCountMap && 0 < timeCountMap.size()) {
          Iterator<String> iterator1 = timeCountMap.keySet().iterator();
          while (iterator1.hasNext()) {
            String time = iterator1.next();
            AtomicInteger atomicInteger = timeCountMap.get(time);
            Integer value = atomicInteger.intValue();
            Integer count = countMap1.get(time);
            if (null == count) {
              countMap1.put(time, value);
              maxValue1 = Math.max(maxValue1, value);
            } else {
              int tempValue = value + count;
              countMap1.put(time, tempValue);
              maxValue1 = Math.max(maxValue1, tempValue);
            }
          }
        }
      }
    }

    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    ObjectNode jsonObject = JsonUtil.createJSONObject();
    jsonObject.put("maxQps1", maxValue1);

    // return bySuccess;
    // Map<String, Integer> countMap2 = new HashMap<>();
    // Integer maxValue2 = -1;
    // Map<String, AtomicInteger> timeCountMap = StatisticsConsumeProcessorThreadQPS.getTimeCountMap();
    // // ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    // if (null != timeCountMap && 0 < timeCountMap.size()) {
    //   Iterator<String> iterator = timeCountMap.keySet().iterator();
    //   while (iterator.hasNext()) {
    //     String key = iterator.next();
    //     AtomicInteger atomicInteger = timeCountMap.get(key);
    //     Integer value = atomicInteger.intValue();
    //     countMap2.put(key, value);
    //     maxValue2 = Math.max(maxValue2, value);
    //   }
    // }
    // if(!maxValue1.equals(maxValue2)){
    //   jsonObject.put("detail1", JsonUtil.obj2String(countMap1));
    //   jsonObject.put("maxValue2", maxValue2);
    //   jsonObject.put("detail2", JsonUtil.obj2String(countMap2));
    // }

    bySuccess.setData(jsonObject.toString());
    return bySuccess;
  }

  /**
   * <B>方法名称：updateSkywalkingAgent</B>
   * <B>概要说明：更新探针别名</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月29日 14:06:30
   * @Param [agentCode, pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/updateSkywalkingAgent", method = RequestMethod.GET)
  public ServerResponse<String> updateSkywalkingAgent(
    @RequestParam(value = "id") Integer id,
    @RequestParam(value = "agentName") String agentName) {
    ServerResponse<String> bySuccess = msAgentInformationService.updateSkywalkingAgent(id, agentName);
    return bySuccess;
  }

  /**
   * <B>方法名称：getAllSkywalkingAgent</B>
   * <B>概要说明：从数据库中获取所有的探针信息</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月29日 10:06:11
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllSkywalkingAgent", method = RequestMethod.GET)
  public ServerResponse<String> getAllSkywalkingAgent(
    String agentCode,
    @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
    @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {

    ServerResponse<String> bySuccess = msAgentInformationService.getAllSkywalkingAgent(agentCode, pageNo, pageSize);
    return bySuccess;
  }

  /**
   * <B>方法名称：getActiveSkywalkingAgent</B>
   * <B>概要说明：获取存活的探针</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月27日 14:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getActiveSkywalkingAgent", method = RequestMethod.GET)
  public ServerResponse<String> getActiveSkywalkingAgent() {
    return msAgentInformationService.getActiveSkywalkingAgent();
  }

  /**
   * <B>方法名称：getUserPortraitRules</B>
   * <B>概要说明：获取所有的规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月23日 15:06:53
   * @Param [pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getUserPortraitRules", method = RequestMethod.GET)
  public ServerResponse<String> getUserPortraitRules(@RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                     @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return userPortraitRulesService.getAllUserPortraitRules(pageNo, pageSize);
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTimeRule</B>
   * <B>概要说明：禁启用用户在什么时间访问了多少次系统规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 17:06:55
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/updateUserPortraitRule", method = RequestMethod.POST)
  public ServerResponse<String> updateUserPortraitRule(@RequestParam(value = "ruleId") Integer ruleId, @RequestParam(value = "isDelete") Integer isDelete) {
    return userPortraitRulesService.updateUserPortraitRule(ruleId, isDelete);
  }

  /**
   * <B>方法名称：initAllTableNameFields</B>
   * <B>概要说明：如果前端没有传递数据库名称，那么根据调用链信息获取到有哪些数据库，然后根据数据库名称获取所有的表，最后根据表获取对应的所有的字段，最后将获取到的表和相应的字段保存到数据库中</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月20日 14:06:37
   * @Param dbName：数据库名称，选传。如果没有传递，那么从调用链信息中获取数据库名称列表。
   **/
  @ResponseBody
  @RequestMapping(value = "/initAllTableNameFields", method = RequestMethod.GET)
  public ServerResponse<String> initAllTableNameFields(String dbName) {
    ServerResponse<String> allTableNames = msThirdPartyTableListService.getAllTableNames(dbName);
    if (allTableNames.getCode().equals(AiitExceptionCode.SUCCESS.getCode())) {
      String data = allTableNames.getData();
      Map<String, String> dbNameTableMap = JsonUtil.string2Obj(data, Map.class);
      msThirdPartyTableFieldsService.getAllTableFieldsName(dbNameTableMap);
    }
    return allTableNames;
  }

  /**
   * <B>方法名称：updateSpecificDbTableNameFields</B>
   * <B>概要说明：更新指定的数据库中某个表的字段</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月20日 14:06:37
   * @Param
   **/
  @ResponseBody
  @RequestMapping(value = "/updateSpecificDbTableNameFields", method = RequestMethod.POST)
  public ServerResponse<String> updateSpecificDbTableNameFields(@RequestParam(value = "id") Integer id,
                                                                String field,
                                                                String fieldName,
                                                                String fieldNote) {
    return msThirdPartyTableFieldsService.updateSpecificDbTableNameFields(id, field, fieldName, fieldNote);
  }

  /**
   * <B>方法名称：getSpecificTableNameFields</B>
   * <B>概要说明：获取指定的数据库中某个表有哪些字段</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月20日 14:06:37
   * @Param
   **/
  @ResponseBody
  @RequestMapping(value = "/getSpecificDbTableNameFields", method = RequestMethod.GET)
  public ServerResponse<String> getSpecificDbTableNameFields(String dbName,
                                                             String tableName,
                                                             @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                             @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msThirdPartyTableFieldsService.getSpecificDbTableNameFields(dbName, tableName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAllTableNameFromDMS</B>
   * <B>概要说明：获取所有数据库表名</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 17:06:51
   * @Param [dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllTableNameFromDMS", method = RequestMethod.GET)
  public ServerResponse<String> getAllTableNameFromDMS() {
    return auditLogService.getAllTableNameFromDMS();
  }

  /**
   * <B>方法名称：getAllSqlTypeFromDMS</B>
   * <B>概要说明：获取所有sql语句的操作类型</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 17:06:51
   * @Param [dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllSqlTypeFromDMS", method = RequestMethod.GET)
  public ServerResponse<String> getAllSqlTypeFromDMS() {
    return auditLogService.getAllSqlTypeFromDMS();
  }

  /**
   * <B>方法名称：getAllUserNameFromDMS</B>
   * <B>概要说明：获取所有操作DMS的用户名</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 17:06:51
   * @Param [dbUserName, sqlType, msTableName, startTime, endTime, pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllUserNameFromDMS", method = RequestMethod.GET)
  public ServerResponse<String> getAllUserNameFromDMS() {
    return auditLogService.getAllUserNameFromDMS();
  }

  /**
   * <B>方法名称：getDmsAuditLogFromDb</B>
   * <B>概要说明：从数据库中获取DMS的数据库审计日志</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月15日 15:06:08
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getDmsAuditLogFromDb", method = RequestMethod.GET)
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
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getUserNameAnomalyDetectionInfo", method = RequestMethod.GET)
  public ServerResponse<String> getUserNameAnomalyDetectionInfo() {
    return msAlarmInformationService.getUserNameAnomalyDetectionInfo();
  }


  /**
   * <B>方法名称：getAllAlarmInfoDetailByUserName</B>
   * <B>概要说明：获取指定用户所有的异常信息</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月24日 16:06:19
   * @Param [userName, pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllAlarmInfoDetailByUserName", method = RequestMethod.GET)
  public ServerResponse<String> getAllAlarmInfoDetailByUserName(@RequestParam(value = "userName") String userName,
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
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAnomalyDetectionInfo", method = RequestMethod.GET)
  public ServerResponse<String> getAnomalyDetectionInfo(String userName,
                                                        @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                        @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAlarmInformationService.getAllAlarmInfo(userName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAnomalyDetectionInfoByGroupByUserName</B>
   * <B>概要说明：根据用户名分组获取告警摘要信息</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月30日 09:06:06
   * @Param [pageNo, pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAnomalyDetectionInfoByGroupByUserName", method = RequestMethod.GET)
  public ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(
    @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
    @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return msAlarmInformationService.getAnomalyDetectionInfoByGroupByUserName(pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAllUserNamePortraitByVisitedTime</B>
   * <B>概要说明：获取所有的用户</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllUserNamePortraitByVisitedTime", method = RequestMethod.GET)
  public ServerResponse<String> getAllUserNamePortraitByVisitedTime() {
    return userPortraitByTimeService.getAllUserNamePortraitByVisitedTime();
  }

  /**
   * <B>方法名称：getUserPortraitByVisitedTime</B>
   * <B>概要说明：获取用户在什么时间访问了多少次系统</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllUserPortraitByVisitedTime", method = RequestMethod.GET)
  public ServerResponse<String> getAllUserPortraitByVisitedTime(String userName, /* 登录系统的名称 */
                                                                @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return userPortraitByTimeService.getAllUserPortraitByVisitedTime(userName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAllVisitedTablePortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取所有的表名</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllVisitedTablePortraitByVisitedTableEveryday", method = RequestMethod.GET)
  public ServerResponse<String> getAllVisitedTablePortraitByVisitedTableEveryday() {
    return userPortraitByTableService.getAllVisitedTablePortraitByVisitedTableEveryday();
  }

  /**
   * <B>方法名称：getUserPortraitByVisitedTime</B>
   * <B>概要说明：获取用户在什么时间访问了多少次系统</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getUserPortraitByVisitedTime", method = RequestMethod.GET)
  public ServerResponse<String> getUserPortraitByVisitedTime() {
    return userPortraitByTableService.getAllVisitedTablePortraitByVisitedTableEveryday();
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTimeRule</B>
   * <B>概要说明：禁启用用户在什么时间访问了多少次系统规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 17:06:55
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/updateUserPortraitByVisitedTimeRule", method = RequestMethod.GET)
  public ServerResponse<String> updateUserPortraitByVisitedTimeRule(@RequestParam(value = "ruleId") Integer ruleId, @RequestParam(value = "isDelete") Integer isDelete) {
    return userPortraitByTimeService.updateUserPortraitByVisitedTimeRule(ruleId, isDelete);
  }

  /**
   * <B>方法名称：getAllUserNameUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：获取所有的用户名</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllUserNameUserPortraitByVisitedTableEveryday", method = RequestMethod.GET)
  public ServerResponse<String> getAllUserNameUserPortraitByVisitedTableEveryday() {
    return userPortraitByTableService.getAllUserNameUserPortraitByVisitedTableEveryday();
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTableEveryday</B>
   * <B>概要说明：更新用户访问过的表的画像规则：isDelete=0，启用这个规则；isDelete=1，禁用这个规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 14:06:35
   * @Param [ruleId, isDelete]
   **/
  @ResponseBody
  @RequestMapping(value = "/updateUserPortraitByVisitedTableEverydayRule", method = RequestMethod.GET)
  public ServerResponse<String> updateUserPortraitByVisitedTableEverydayRule(@RequestParam(value = "ruleId") Integer ruleId, @RequestParam(value = "isDelete") Integer isDelete) {
    return userPortraitByTableService.updateUserPortraitByVisitedTableEverydayRule(ruleId, isDelete);
  }

  /**
   * <B>方法名称：addUserPortraitByVisitedTtimeRule</B>
   * <B>概要说明：增加用户在什么时间访问过系统多少次的画像规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 17:41:35
   * @Param [ruleId, isDelete]
   **/
  @ResponseBody
  @RequestMapping(value = "/addUserPortraitByVisitedTtimeRule", method = RequestMethod.GET)
  public ServerResponse<String> addUserPortraitByVisitedTtimeRule(@RequestParam(value = "userName") String userName,
                                                                  @RequestParam(value = "forenoonCount") Integer forenoonCount,
                                                                  @RequestParam(value = "afternoonCount") Integer afternoonCount,
                                                                  @RequestParam(value = "nightCount") Integer nightCount) {
    return userPortraitByTimeService.addUserPortraitByVisitedTtimeRule(userName, forenoonCount, afternoonCount, nightCount);
  }

  /**
   * <B>方法名称：addUserPortraitByVisitedTableEverydayRule</B>
   * <B>概要说明：增加用户访问过的表的画像规则</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 16:41:35
   * @Param [ruleId, isDelete]
   **/
  @ResponseBody
  @RequestMapping(value = "/addUserPortraitByVisitedTableEverydayRule", method = RequestMethod.GET)
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
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月13日 09:06:57
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getUserPortraitByVisitedTableEveryday", method = RequestMethod.GET)
  public ServerResponse<String> getUserPortraitByVisitedTableEveryday(String userName, /* 登录系统的名称 */
                                                                      String visitedTable, /* 访问数据库的表 */
                                                                      String visitedDate, /* 访问数据库表的时间 */
                                                                      String visitedDbInstance,/*访问的数据库*/
                                                                      @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                      @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {


    return userPortraitByTableService.getUserPortraitByVisitedTableEveryday(userName, visitedTable, visitedDate, visitedDbInstance,pageNo, pageSize);
  }


  /**
   * <B>方法名称：userPortraitByVisitedTable</B>
   * <B>概要说明：基于历史数据，统计用户每天访问表的次数，以此来生成用户的画像</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月08日 14:06:08
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/userPortraitByVisitedTableEveryday", method = RequestMethod.GET)
  public ServerResponse<String> userPortraitByVisitedTableEveryday() {
    return userPortraitByTableService.createUserPortraitByVisitedTableEveryday();
  }

  /**
   * <B>方法名称：userPortraitByVisitedTime</B>
   * <B>概要说明：基于历史数据，统计用户访问系统的时间，以此来生成用户的画像</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月07日 14:06:08
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/userPortraitByVisitedTime", method = RequestMethod.GET)
  public ServerResponse<String> userPortraitByVisitedTime() {
    return userPortraitByTimeService.createUserPortraitByVisitedTime();
  }

  /**
   * <B>方法名称：getBehaviorByUserName/B>
   * <B>概要说明：基于用户的访问行为</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年05月30日 14:30:19
   * @Param [sqlType, applicationUserName, pageNo,pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getBehaviorByUserName", method = RequestMethod.GET)
  public ServerResponse<String> getBehaviorByUserName(String applicationUserName, String sqlType, Integer pageNo, Integer pageSize) {
    return auditLogService.getBehaviorByUserName(applicationUserName, sqlType, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getBehaviorByOptTime/B>
   * <B>概要说明：基于时间的访问行为</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年06月1日 10:30:19
   * @Param [sqlInsightDbUserName, optTime, pageNo,pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getBehaviorByOptTime", method = RequestMethod.GET)
  public ServerResponse<String> getBehaviorByOptTime(String sqlType, String startTime, String endTime, Integer pageNo, Integer pageSize) {
    return auditLogService.getBehaviorByOptTime(sqlType, startTime, endTime, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getBehaviorByOptTime/B>
   * <B>概要说明：基于库表的访问行为</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年06月1日 14:30:19
   * @Param [sqlInsightDbUserName, optTime, pageNo,pageSize]
   **/
  @ResponseBody
  @RequestMapping(value = "/getBehaviorByTableName", method = RequestMethod.GET)
  public ServerResponse<String> getBehaviorByTableName(String msTableName, Integer pageNo, Integer pageSize) {
    return auditLogService.getBehaviorByTableName(msTableName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getNumberOfTablesByOpTime/B>
   * <B>概要说明：基于时间段获取不同表的操作次数</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年06月1日 14:30:19
   * @Param
   **/
  @ResponseBody
  @RequestMapping(value = "/getNumberOfTablesByOpTime", method = RequestMethod.GET)
  public ServerResponse<String> getNumberOfTablesByOpTime(String msTableName, String startTime, String endTime, Integer pageNo, Integer pageSize) {
    return auditLogService.getNumberOfTablesByOpTime(msTableName, startTime, endTime, pageNo, pageSize);
  }

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
  @RequestMapping(value = "/getAllUserName", method = RequestMethod.GET)
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
  @RequestMapping(value = "/getAllMsTableName", method = RequestMethod.GET)
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
  @RequestMapping(value = "/getAllInstanceAndTableName", method = RequestMethod.GET)
  public ServerResponse<InstanceTable> getAllInstanceAndTableName() {
    return segmentDetailService.getAllInstanceAndTableName();
  }


  /**
   * <B>方法名称：getAllInstanceTrueName/B>
   * <B>概要说明：获取数据库表名与汉语名字的对应关系</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllInstanceTrueName", method = RequestMethod.GET)
  public ServerResponse<MsThirdPartyTableListDo> getAllInstanceTrueName() {
    return segmentDetailService.getAllInstanceTrueName();
  }

  /**
   * <B>方法名称：getCoarseCountsOfUser/B>
   * <B>概要说明：获取用户对数据的粗粒度信息</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/

  @ResponseBody
  @RequestMapping(value = "/getCoarseCountsOfUser", method = RequestMethod.GET)
  public ServerResponse<List<UserCoarseInfo>>getCoarseCountsOfUser(@RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                   @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {


    return segmentDetailService.getCoarseCountsOfUser(pageNo, pageSize);

  }

  /**
   * <B>方法名称：getCoarseCountsOfTableName/B>
   * <B>概要说明：获取数据库的粗粒度信息</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/

  @ResponseBody
  @RequestMapping(value = "/getCoarseCountsOfTableName", method = RequestMethod.GET)
  public ServerResponse<List<TableCoarseInfo>>getCoarseCountsOfTableName(@RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                                                   @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {


    return segmentDetailService.getCoarseCountsOfTableName(pageNo, pageSize);

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
  @RequestMapping(value = "/getCoarseCountsOfOneUser", method = RequestMethod.GET)
  public ServerResponse<UserCoarseInfo>getCoarseCountsOfOneUser( @RequestParam(value = "applicationUserName", defaultValue = "") String applicationUserName, /* 登录系统的名称 */
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
  @RequestMapping(value = "/getCountsOfUser", method = RequestMethod.GET)
  public ServerResponse<Long>getCountsOfUser(String applicationUserName, /* 登录系统的名称 */
                                               String dbUserName, /* 访问数据库的用户名 */
                                               String dbType, /* SQL语句的类型；是insert、select、update、delete等 */
                                               String msTableName, /* 数据库表名 */
                                               String startTime, /* 开始时间 */
                                               String endTime, /* 结束时间 */
                                               @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                               @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {

    return segmentDetailService.getCountsOfUser(applicationUserName, dbType, msTableName, startTime, endTime, dbUserName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getCountsOfUserRecentSevenDays/B>
   * <B>概要说明：获取用户对数据的详细数据</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/

  @ResponseBody
  @RequestMapping(value = "/getCountsOfUserRecentSevenDays", method = RequestMethod.GET)
  public ServerResponse<List<Long>>getCountsOfUserRecentSevenDays(String applicationUserName, /* 登录系统的名称 */
                                             String dbUserName, /* 访问数据库的用户名 */
                                             String dbType, /* SQL语句的类型；是insert、select、update、delete等 */
                                             String msTableName, /* 数据库表名 */
                                             String startTime, /* 开始时间 */
                                             String endTime, /* 结束时间 */
                                             @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                             @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) throws ParseException {


    return segmentDetailService.getCountsOfUserUserRecentSevenDays(applicationUserName, dbType, msTableName, startTime, endTime, dbUserName, pageNo, pageSize);
  }


  /**
   * <B>方法名称：getCountsOfAllRecentSevenDays/B>
   * <B>概要说明：获取用户对数据的详细数据</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/

  @ResponseBody
  @RequestMapping(value = "/getCountsOfAllRecentSevenDays", method = RequestMethod.GET)
  public ServerResponse<List<Long>> getCountsOfAllRecentSevenDays(String startTime, /* 开始时间 */String endTime /* 结束时间 */) {
    return segmentDetailService.getCountsOfAllRecentSevenDays(startTime,endTime);
  }

  /**
   * <B>方法名称：getOverviewOfSystem/B>
   * <B>概要说明：获取可视化系统关于、管理信息数量、用户数量的总览信息</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author lhx
   * @Date 2022年07月5日 14:30:19
   * @Param
   **/

  @ResponseBody
  @RequestMapping(value = "/getOverviewOfSystem", method = RequestMethod.GET)
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
  @RequestMapping(value = "/getUserUsualAndUnusualData", method = RequestMethod.GET)
  public ServerResponse<List<UserUsualAndUnusualVisitedData>> getUserUsualAndUnusualData(String applicationUserName) {
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
  @RequestMapping(value = "/getAlarmData", method = RequestMethod.GET)
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
  @RequestMapping(value = "/getUserAlarmData", method = RequestMethod.GET)
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
  @RequestMapping(value = "/getAllSegments", method = RequestMethod.GET)
  public ServerResponse<String> getAllSegments(String applicationUserName, /* 登录系统的名称 */
                                               String dbUserName, /* 访问数据库的用户名 */
                                               String dbType, /* SQL语句的类型；是insert、select、update、delete等 */
                                               String msTableName, /* 数据库表名 */
                                               String startTime, /* 开始时间 */
                                               String endTime, /* 结束时间 */
                                               @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo,
                                               @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return segmentDetailService.getAllSegmentsBySegmentRelation(applicationUserName, dbType, msTableName, startTime, endTime, dbUserName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAuditLogFromExcel</B>
   * <B>概要说明：从excel表格中获取审计日志</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 19:05:00
   * @Param []
   **/
  @ResponseBody
  @RequestMapping(value = "/getAuditLogFromExcel", method = RequestMethod.GET)
  public ServerResponse<String> getAuditLogFromExcel(@RequestParam("path") String path) {
    return auditLogService.getAuditlogByExcel(path);
  }

  /**
   * <B>方法名称：</B>
   * <B>概要说明：</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年05月26日 16:05:50
   * @Param [startTime, endTime]
   **/
  @ResponseBody
  @RequestMapping(value = "/autoFetchAuditlogByDMS", method = RequestMethod.GET)
  public ServerResponse<String> autoFetchAuditlogByDMS(@RequestParam("startTime") String startTime, @RequestParam("endTime") String endTime) {
    return auditLogService.autoFetchAuditlogByDMS(startTime, endTime);
  }

}
