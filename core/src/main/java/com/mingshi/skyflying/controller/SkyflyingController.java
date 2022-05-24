package com.mingshi.skyflying.controller;

import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
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
}
