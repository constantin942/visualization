package com.mingshi.skyflying.impl;


import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.SegmentRelationDao;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.JsonUtil;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.LinkedList;
import java.util.List;

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
  private SegmentDao segmentDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;

  @Override
  public ServerResponse<String> getAllSegments() {
    List<SegmentDo> segments = segmentDao.selectAll();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(segments));
    return bySuccess;
  }

  @Override
  public ServerResponse<String> getAllSegmentsBySegmentRelation() {
    // 查询出所有的traceId；2022-04-25 09:56:29
    List<String> globalTraceIdList = segmentRelationDao.selectAllGlobalTraceId();
    LinkedList<JSONObject> linkedList = new LinkedList<>();
    for (String globalTraceId : globalTraceIdList) {
    //  根据traceId从segment表中查询数据；2022-04-25 09:57:02
      List<SegmentDo> segmentDos = segmentDao.selectByGlobalTraceId(globalTraceId);
      for (SegmentDo segmentDo : segmentDos) {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("id",segmentDo.getId() );
        jsonObject.put("operationName",segmentDo.getOperationName());
        jsonObject.put("requestStartTime",segmentDo.getRequestStartTime());
        jsonObject.put("reorganizingSpans",segmentDo.getReorganizingSpans());
        linkedList.add(jsonObject);
      }
    }
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(linkedList));
    return bySuccess;
  }
}
