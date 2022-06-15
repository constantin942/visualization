package com.mingshi.skyflying.impl;


import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
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
  private UserTokenDao userTokenDao;
  @Resource
  private SegmentRelationDao segmentRelationDao;
  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;

  @Override
  public ServerResponse<String> getAllSegmentsBySegmentRelation2(String applicationUserName, String dbType, String msTableName, String startTime, String endTime, String dbUserName, Integer pageNo, Integer pageSize) {
    log.info("开始执行 # SegmentDetailServiceImpl.getAllSegmentsBySegmentRelation2() # 获取用户的调用链信息。");
    Map<String, Object> map = new HashMap<>();
    if (StringUtil.isNotBlank(applicationUserName)) {
      map.put("applicationUserName", applicationUserName);
    }
    if (StringUtil.isNotBlank(dbType)) {
      map.put("dbType", dbType);
    }
    if (StringUtil.isNotBlank(msTableName)) {
      map.put("msTableName", msTableName);
    }
    if (StringUtil.isNotBlank(startTime)) {
      map.put("startTime", startTime);
    }
    if (StringUtil.isNotBlank(endTime)) {
      map.put("endTime", endTime);
    }
    if (StringUtil.isNotBlank(dbUserName)) {
      map.put("dbUserName", dbUserName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    map.put("pageNo", (pageNo - 1) * pageSize);
    map.put("pageSize", pageSize);

    // 从数据库中获取一次调用链中所涉及到的segment信息；2022-06-02 17:41:11
    LinkedHashMap<String/* global_trace_id */, LinkedHashMap<String/* url */, List<MsSegmentDetailDo>>> hashMap = getSegmentDetailsFromDb(map);

    // 组装每一条调用链信息；2022-06-02 17:41:16
    String traceInfo = getEveryCallChainInfo(hashMap);


    Long count = msSegmentDetailDao.selectCountAll(map);
    Map<String, Object> context = new HashMap<>();
    context.put("rows", JsonUtil.obj2String(traceInfo));
    context.put("total", count);
    log.info("执行完毕 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", JsonUtil.obj2String(context));
  }

  /**
   * <B>方法名称：getAllUserName</B>
   * <B>概要说明：获取用户登录系统的用户名</B>
   *
   * @return
   * @Author zm
   * @Date 2022年06月06日 15:06:00
   * @Param
   **/
  @Override
  public ServerResponse<String> getAllUserName() {
    List<String> list = msSegmentDetailDao.selectAllUserName();
    ServerResponse serverResponse = ServerResponse.createBySuccess();
    serverResponse.setData(JsonUtil.obj2String(list));
    return serverResponse;
  }

  /**
   * <B>方法名称：getAllMsTableName</B>
   * <B>概要说明：获取数据库名</B>
   *
   * @return
   * @Author zm
   * @Date 2022年06月06日 15:06:00
   * @Param
   **/
  @Override
  public ServerResponse<String> getAllMsTableName() {
    List<String> list = msSegmentDetailDao.selectAllMsTableName();
    ServerResponse serverResponse = ServerResponse.createBySuccess();
    serverResponse.setData(JsonUtil.obj2String(list));
    return serverResponse;
  }

  /**
   * <B>方法名称：getSegmentDetailsFromDb</B>
   * <B>概要说明：从数据库中获取一次调用链中所涉及到的segment信息</B>
   *
   * @return java.util.HashMap<java.lang.String, java.util.HashMap < java.lang.String, java.util.List < com.mingshi.skyflying.domain.MsSegmentDetailDo>>>
   * @Author zm
   * @Date 2022年06月02日 17:06:01
   * @Param [map]
   **/
  private LinkedHashMap<String, LinkedHashMap<String, List<MsSegmentDetailDo>>> getSegmentDetailsFromDb(Map<String, Object> map) {
    LinkedHashMap<String/* global_trace_id */, LinkedHashMap<String/* url */, List<MsSegmentDetailDo>>> linkedHashMap = new LinkedHashMap<>();
    try {
      List<MsSegmentDetailDo> msSegmentDetailDoList = msSegmentDetailDao.selectAll(map);
      log.info("# SegmentDetailServiceImpl.getSegmentDetailsFromDb() # 根据查询条件 = 【{}】，在表ms_segment_detail中获取到了【{}】条详细数据。", JsonUtil.obj2String(map), msSegmentDetailDoList.size());
      if (null != msSegmentDetailDoList && 0 < msSegmentDetailDoList.size()) {
        for (MsSegmentDetailDo msSegmentDetailDo : msSegmentDetailDoList) {
          String globalTraceId = msSegmentDetailDo.getGlobalTraceId();
          LinkedHashMap<String, List<MsSegmentDetailDo>> globalTraceIdHashMap = linkedHashMap.get(globalTraceId);
          if (null == globalTraceIdHashMap) {
            globalTraceIdHashMap = new LinkedHashMap<>();
            linkedHashMap.put(globalTraceId, globalTraceIdHashMap);
          }
          String operationName = msSegmentDetailDo.getOperationName();
          List<MsSegmentDetailDo> msSegmentDetailDosList = globalTraceIdHashMap.get(operationName);
          if (null == msSegmentDetailDosList) {
            msSegmentDetailDosList = new LinkedList<>();
            globalTraceIdHashMap.put(operationName, msSegmentDetailDosList);
          }
          String parentSegmentId = msSegmentDetailDo.getParentSegmentId();
          if (StringUtil.isBlank(parentSegmentId)) {
            // 把最顶级的segment放入list的第一个位置；2022-06-02 18:07:31
            msSegmentDetailDosList.add(0, msSegmentDetailDo);
          } else {
            msSegmentDetailDosList.add(msSegmentDetailDo);
          }
        }
      }
    } catch (Exception e) {
      log.error("# SegmentDetailServiceImpl.getSegmentDetailsFromDb() # 从数据库中获取一次调用链中所涉及到的segment信息时，出现了异常。", e);
    }
    return linkedHashMap;
  }

  /**
   * <B>方法名称：getEveryCallChainInfo</B>
   * <B>概要说明：组装每一条调用链信息</B>
   *
   * @return java.util.List<java.lang.String>
   * @Author zm
   * @Date 2022年06月02日 17:06:31
   * @Param [hashMap]
   **/
  private String getEveryCallChainInfo(LinkedHashMap<String, LinkedHashMap<String, List<MsSegmentDetailDo>>> hashMap) {
    HashSet<JSONObject> hashSet = new HashSet<>();
    try {
      Iterator<String> iterator1 = hashMap.keySet().iterator();
      while (iterator1.hasNext()) {
        JSONObject everyGlobalCallInfoJson = new JSONObject();
        hashSet.add(everyGlobalCallInfoJson);
        JSONObject headerJson = new JSONObject();
        JSONArray bodyJsonArray = new JSONArray();
        // JSONObject bodyJson = new JSONObject();
        String globalTraceId = iterator1.next();
        // 组装每一个调用链；2022-06-02 15:03:11
        HashMap<String, List<MsSegmentDetailDo>> urlHhashMap = hashMap.get(globalTraceId);
        Iterator<String> iterator2 = urlHhashMap.keySet().iterator();
        while (iterator2.hasNext()) {
          String url = iterator2.next();
          List<MsSegmentDetailDo> segmentDetailDoList = urlHhashMap.get(url);
          if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {
            JSONObject jsonObject = new JSONObject();
            LinkedList<JSONObject> everyBodylinkedList = new LinkedList<>();
            jsonObject.put("segments", everyBodylinkedList);
            bodyJsonArray.add(jsonObject);
            everyGlobalCallInfoJson.put("body",bodyJsonArray);
            for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailDoList) {
              String parentSegmentId = msSegmentDetailDo.getParentSegmentId();
              if (StringUtil.isBlank(parentSegmentId) && 0 == headerJson.size()) {
                headerJson.put("userName", msSegmentDetailDo.getUserName());
                headerJson.put("url", msSegmentDetailDo.getOperationName());
                headerJson.put("requestStartTime", msSegmentDetailDo.getStartTime());
                everyGlobalCallInfoJson.put("header", headerJson);
              }
              jsonObject.put("url", msSegmentDetailDo.getOperationName());
              JSONObject detailJson = new JSONObject();
              detailJson.put("peer", msSegmentDetailDo.getPeer());
              detailJson.put("serviceInstanceName", msSegmentDetailDo.getServiceInstanceName());
              detailJson.put("serviceCode", msSegmentDetailDo.getServiceCode());
              detailJson.put("dbType", msSegmentDetailDo.getDbType());
              detailJson.put("dbInstance", msSegmentDetailDo.getDbInstance());
              detailJson.put("dbUserName", msSegmentDetailDo.getDbUserName());
              detailJson.put("dbStatement", msSegmentDetailDo.getDbStatement());
              everyBodylinkedList.add(detailJson);
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("# SegmentDetailServiceImpl.getEveryCallChainInfo() # 组装每一条调用链信息时，出现了异常。", e);
    }
    if(0 < hashSet.size()){
      return JsonUtil.obj2String(hashSet);
    }
    return null;
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
          Map<String, Object> queryMap2 = new HashMap<>();
          queryMap2.put("globalTraceId", globalTraceId);
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
