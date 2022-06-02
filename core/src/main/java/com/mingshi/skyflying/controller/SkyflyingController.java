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
