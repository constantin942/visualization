package com.mingshi.skyflying.impl;


import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.SegmentDao;
import com.mingshi.skyflying.dao.SegmentRelationDao;
import com.mingshi.skyflying.dao.UserTokenDao;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.domain.SegmentRelationDo;
import com.mingshi.skyflying.domain.UserTokenDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.SegmentDetailService;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;

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
  private UserTokenDao userTokenDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;

  @Override
  public ServerResponse<String> getAllSegmentsBySegmentRelation(String userName, Integer pageNo, Integer pageSize) {
    List<Map<String, String>> globalTraceIdMapList = getGlobalTraceIdList(userName, pageNo, pageSize);
    log.info("开始执行 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");

    LinkedList<JSONObject> allLinkedList = new LinkedList<>();
    for (Map<String, String> globalTraceIdMap : globalTraceIdMapList) {
      LinkedList<JSONObject> linkedList = new LinkedList<>();
      JSONObject returnJson = new JSONObject();

      String segmentIdList = globalTraceIdMap.get("segmentIds");
      String globalTraceId = globalTraceIdMap.get("globalTraceId");
      UserTokenDo userTokenDo = userTokenDao.selectByGlobalTraceId(globalTraceId);
      if (null == userTokenDo) {
        log.error("根据全局追踪id = 【{}】，在用户token表中没有找到数据。", globalTraceId);
        continue;
      }

      JSONArray segmentIdArray = JSONObject.parseArray(segmentIdList);
      for (Object segmentIds : segmentIdArray) {
        String segmentIdStr = String.valueOf(segmentIds);
        JSONObject jsonObject = JSONObject.parseObject(segmentIdStr);
        String segmentId = jsonObject.getString("currentSegmentId");
        String parentSegmentId = jsonObject.getString("parentSegmentId");
        //  根据traceId从segment表中查询数据；2022-04-25 09:57:02
        SegmentDo segmentDo = segmentDao.selectBySegmentId(segmentId);
        if (null == segmentDo) {
          continue;
        }
        JSONObject paramsJsonObject = new JSONObject();
        if (parentSegmentId.equals("##")) {
          JSONObject headSegmentJson = new JSONObject();
          headSegmentJson.put("userName", userTokenDo.getUserName());
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
      if (0 < linkedList.size()) {
        returnJson.put("body", linkedList);
      }
      if (0 < returnJson.size()) {
        allLinkedList.add(returnJson);
      }
    }
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(allLinkedList));
    log.info("执行完毕 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
    return bySuccess;
  }

  /**
   * <B>方法名称：getGlobalTraceIdList</B>
   * <B>概要说明：根据用户名，获取对应全局追踪id</B>
   *
   * @return java.util.List<java.util.Map < java.lang.String, java.lang.String>>
   * @Author zm
   * @Date 2022年05月17日 17:05:04
   * @Param [userName]
   **/
  private List<Map<String, String>> getGlobalTraceIdList(String userName, Integer pageNo, Integer pageSize) {
    List<Map<String, String>> globalTraceIdMapList = new ArrayList<>();
    Map<String, Object> queryMap = new HashMap<>();
    // 设置默认值；2022-05-18 17:27:15
    if (null == pageNo) {
      queryMap.put("pageNo", 1);
    }
    if (null == pageSize) {
      queryMap.put("pageSize", 10);
    }
    // 查询出所有的traceId；2022-04-25 09:56:29
    if (StringUtil.isNotBlank(userName)) {
      queryMap.put("userName", userName);
      List<UserTokenDo> listUserToken = userTokenDao.selectByUserName(queryMap);
      if (null != listUserToken && 0 < listUserToken.size()) {
        for (UserTokenDo userTokenDo : listUserToken) {
          String globalTraceId = userTokenDo.getGlobalTraceId();
          Map<String,Object> queryMap2 = new HashMap<>();
          queryMap2.put("globalTraceId",globalTraceId);
          SegmentRelationDo segmentRelationDo = segmentRelationDao.selectByGlobalTraceId(queryMap2);
          Map<String, String> map = new HashMap<>();
          map.put("globalTraceId", segmentRelationDo.getGlobalTraceId());
          map.put("segmentIds", segmentRelationDo.getSegmentIds());
          globalTraceIdMapList.add(map);
        }
      }
    } else {
      globalTraceIdMapList = segmentRelationDao.selectAllGlobalTraceId(queryMap);
    }
    return globalTraceIdMapList;
  }
}
