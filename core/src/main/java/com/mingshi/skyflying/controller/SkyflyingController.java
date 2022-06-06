package com.mingshi.skyflying.controller;

import com.mingshi.skyflying.dao.MsAuditLogDao;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.service.SegmentDetailService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.annotation.Resource;

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
  private MsAuditLogDao msAuditLogDao;

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
    return auditLogService.getAllUserName();
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
    return auditLogService.getAllMsTableName();
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
   * <B>方法名称：getAllSegments1</B>
   * <B>概要说明：获取所有的访问链条信息，版本1的实现</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author zm
   * @Date 2022年04月19日 17:04:19
   * @Param [request, userName, password]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllSegments1", method = RequestMethod.GET)
  public ServerResponse<String> getAllSegments1(String userName, @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo, @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return segmentDetailService.getAllSegmentsBySegmentRelation1(userName, pageNo, pageSize);
  }

  /**
   * <B>方法名称：getAllSegments2</B>
   * <B>概要说明：获取所有的访问链条信息，版本2的实现</B>
   *
   * @return ServerResponse<SysOperator>
   * @Author zm
   * @Date 2022年06月02日 17:15:19
   * @Param [request, userName, password]
   **/
  @ResponseBody
  @RequestMapping(value = "/getAllSegments2", method = RequestMethod.GET)
  public ServerResponse<String> getAllSegments2(String userName, @RequestParam(value = "pageNo", defaultValue = "1") Integer pageNo, @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize) {
    return segmentDetailService.getAllSegmentsBySegmentRelation2(userName, pageNo, pageSize);
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
