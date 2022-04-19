package com.mingshi.skyflying.impl;


import com.mingshi.skyflying.dao.SegmentDetailDao;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * <B>方法名称：SegmentDetailServiceImpl</B>
 * <B>概要说明：获取用户访问的链条信息</B>
 * @Author zm
 * @Date 2022年04月19日 17:04:57
 * @Param
 * @return
 **/
@Service("segmentDetailService")
public class SegmentDetailServiceImpl implements SegmentDetailService {

  @Resource
  private SegmentDetailDao segmentDetailDao;

  @Override
  public ServerResponse<String> getAllSegments() {
    return null;
  }
}
