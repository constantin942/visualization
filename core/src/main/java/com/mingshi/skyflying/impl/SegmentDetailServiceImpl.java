package com.mingshi.skyflying.impl;


import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.SegmentRelationDao;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.JsonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：SegmentDetailServiceImpl</B>
 * <B>概要说明：获取用户访问的链条信息</B>
 *
 * @Author zm
 * @Date 2022年04月19日 17:04:57
 * @Param
 * @return
 **/
@Slf4j
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
    log.info("开始执行 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
    // 查询出所有的traceId；2022-04-25 09:56:29
    List<Map<String, String>> globalTraceIdMapList = segmentRelationDao.selectAllGlobalTraceId();
    LinkedList<JSONObject> allLinkedList = new LinkedList<>();

    for (Map<String, String> globalTraceIdMap : globalTraceIdMapList) {
      LinkedList<JSONObject> linkedList = new LinkedList<>();
      JSONObject returnJson = new JSONObject();

      String segmentIdList = globalTraceIdMap.get("segmentIds");
      JSONArray segmentIdArray = JSONObject.parseArray(segmentIdList);
      for (Object segmentIds : segmentIdArray) {
        String segmentIdStr = String.valueOf(segmentIds);
        JSONObject jsonObject = JSONObject.parseObject(segmentIdStr);
        String segmentId = jsonObject.getString("currentSegmentId");
        String parentSegmentId = jsonObject.getString("parentSegmentId");
        //  根据traceId从segment表中查询数据；2022-04-25 09:57:02
        SegmentDo segmentDo = segmentDao.selectBySegmentId(segmentId);
        if(null == segmentDo){
          continue;
        }
        JSONObject paramsJsonObject = new JSONObject();
        if (parentSegmentId.equals("##")) {
          JSONObject headSegmentJson = new JSONObject();
          headSegmentJson.put("userName", segmentDo.getUserName());
          headSegmentJson.put("requestStartTime", segmentDo.getRequestStartTime());
          String reorganizingSpans = segmentDo.getReorganizingSpans();
          JSONArray jsonArray = JSONObject.parseArray(reorganizingSpans);
          JSONObject parseObject = JSONObject.parseObject(String.valueOf(jsonArray.get(0)));
          String url = parseObject.getString("url");
          headSegmentJson.put("url", url);
          returnJson.put("header", headSegmentJson);
        }
        paramsJsonObject.put("id", segmentDo.getId());
        paramsJsonObject.put("operationName", segmentDo.getOperationName());
        paramsJsonObject.put("requestStartTime", segmentDo.getRequestStartTime());
        paramsJsonObject.put("reorganizingSpans", segmentDo.getReorganizingSpans());
        linkedList.add(paramsJsonObject);
      }
      if(0 < linkedList.size()){
        returnJson.put("body", linkedList);
      }
      if(0 < returnJson.size()){
        allLinkedList.add(returnJson);
      }
    }
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(allLinkedList));
    log.info("执行完毕 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
    return bySuccess;
  }
}
