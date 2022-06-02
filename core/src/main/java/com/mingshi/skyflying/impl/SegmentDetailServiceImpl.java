package com.mingshi.skyflying.impl;


import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.mingshi.skyflying.dao.*;
import com.mingshi.skyflying.domain.*;
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
  @Resource
  private MsAuditLogDao msAuditLogDao;
  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;

  @Override
  public ServerResponse<String> getAllSegmentsBySegmentRelation1(String userName, Integer pageNo, Integer pageSize) {
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

  @Override
  public ServerResponse<String> getAllSegmentsBySegmentRelation2(String userName, Integer pageNo, Integer pageSize) {
    log.info("开始执行 # SegmentDetailServiceImpl.getAllSegmentsBySegmentRelation2() # 获取用户的调用链信息。");
    Map<String, Object> map = new HashMap<>();
    map.put("applicationUserName", userName);
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    map.put("pageNo", (pageNo - 1) * pageSize);
    map.put("pageSize", pageSize);

    // 从数据库中获取一次调用链中所涉及到的segment信息；2022-06-02 17:41:11
    HashMap<String/* global_trace_id */, HashMap<String/* url */, List<MsSegmentDetailDo>>> hashMap = getSegmentDetailsFromDb(map);

    // 组装每一条调用链信息；2022-06-02 17:41:16
    List<String> returnList = getEveryCallChainInfo(hashMap);

    Long count = msSegmentDetailDao.selectCount(map);
    Map<String, Object> context = new HashMap<>();
    context.put("rows", JsonUtil.obj2String(returnList));
    context.put("total", count);
    log.info("执行完毕 SegmentDetailServiceImpl # getAllSegmentsBySegmentRelation()，获取用户的调用链信息。");
    return ServerResponse.createBySuccess("获取数据成功！", "success", JsonUtil.obj2String(context));
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
  private HashMap<String, HashMap<String, List<MsSegmentDetailDo>>> getSegmentDetailsFromDb(Map<String, Object> map) {
    HashMap<String/* global_trace_id */, HashMap<String/* url */, List<MsSegmentDetailDo>>> hashMap = new HashMap<>();
    try {
      List<MsSegmentDetailDo> msSegmentDetailDoList = msSegmentDetailDao.selectAll(map);
      if (null != msSegmentDetailDoList && 0 < msSegmentDetailDoList.size()) {
        for (MsSegmentDetailDo msSegmentDetailDo : msSegmentDetailDoList) {
          String globalTraceId = msSegmentDetailDo.getGlobalTraceId();
          HashMap<String, List<MsSegmentDetailDo>> globalTraceIdHashMap = hashMap.get(globalTraceId);
          if (null == globalTraceIdHashMap) {
            globalTraceIdHashMap = new HashMap<>();
            hashMap.put(globalTraceId, globalTraceIdHashMap);
          }
          String operationName = msSegmentDetailDo.getOperationName();
          List<MsSegmentDetailDo> msSegmentDetailDosList = globalTraceIdHashMap.get(operationName);
          if (null == msSegmentDetailDosList) {
            msSegmentDetailDosList = new LinkedList<>();
            globalTraceIdHashMap.put(operationName, msSegmentDetailDosList);
          }
          String parentSegmentId = msSegmentDetailDo.getParentSegmentId();
          if(StringUtil.isBlank(parentSegmentId)){
            // 把最顶级的segment放入list的第一个位置；2022-06-02 18:07:31
            msSegmentDetailDosList.add(0,msSegmentDetailDo);
          }else{
            msSegmentDetailDosList.add(msSegmentDetailDo);
          }
        }
      }
    } catch (Exception e) {
      log.error("# SegmentDetailServiceImpl.getSegmentDetailsFromDb() # 从数据库中获取一次调用链中所涉及到的segment信息时，出现了异常。", e);
    }
    return hashMap;
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
  private List<String> getEveryCallChainInfo(HashMap<String, HashMap<String, List<MsSegmentDetailDo>>> hashMap) {
    List<String> returnList = new LinkedList<>();
    try {
      Iterator<String> iterator1 = hashMap.keySet().iterator();
      while (iterator1.hasNext()) {
        String globalTraceId = iterator1.next();
        // 组装每一个调用链；2022-06-02 15:03:11
        HashMap<String, List<MsSegmentDetailDo>> urlHhashMap = hashMap.get(globalTraceId);
        Iterator<String> iterator2 = urlHhashMap.keySet().iterator();
        while (iterator2.hasNext()) {
          String url = iterator2.next();
          List<MsSegmentDetailDo> segmentDetailDoList = urlHhashMap.get(url);
          if (null != segmentDetailDoList && 0 < segmentDetailDoList.size()) {
            JSONObject jsonObject = new JSONObject();
            for (MsSegmentDetailDo msSegmentDetailDo : segmentDetailDoList) {
              String header = jsonObject.getString("header");
              if (StringUtil.isBlank(header)) {
                JSONObject headerJson = new JSONObject();
                headerJson.put("userName", msSegmentDetailDo.getUserName());
                headerJson.put("url", msSegmentDetailDo.getOperationName());
                headerJson.put("requestStartTime", msSegmentDetailDo.getStartTime());
                jsonObject.put("header", headerJson);
              }
              JSONArray jsonArray = jsonObject.getJSONArray("body");
              if (null == jsonArray) {
                jsonArray = new JSONArray();
                jsonObject.put("body", jsonArray);
              }
              JSONObject bodyJson = new JSONObject();
              bodyJson.put("peer", msSegmentDetailDo.getPeer());
              bodyJson.put("serviceInstanceName", msSegmentDetailDo.getServiceInstanceName());
              bodyJson.put("serviceCode", msSegmentDetailDo.getServiceCode());
              bodyJson.put("db_type", msSegmentDetailDo.getDbType());
              bodyJson.put("db_instance", msSegmentDetailDo.getDbInstance());
              bodyJson.put("db_user_name", msSegmentDetailDo.getDbUserName());
              bodyJson.put("db_statement", msSegmentDetailDo.getDbStatement());
              jsonArray.add(bodyJson);
            }
            returnList.add(jsonObject.toJSONString());
          }
        }
      }
    } catch (Exception e) {
      log.error("# SegmentDetailServiceImpl.getEveryCallChainInfo() # 组装每一条调用链信息时，出现了异常。", e);
    }
    return returnList;
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
// todo: 清明节过后，需要将这个实现了。即返给前端时，应该将一条完整的调用链信息返回给前端。2022-06-02 18:13:30
// [{
//     "header": {
//       "userName": "admin",
//         "url": "http://localhost:8080/demo1/test",
//         "requestStartTime": "2022-06-02 17:22:03"
//     },
//     "body": [{
//       "url": "http://localhost:8080/demo1/test",
//         "segments": [{
//         "db_type": "sql",
//           "serviceCode": "dem1",
//           "db_user_name": "root",
//           "peer": "10.0.107.46:3306",
//           "db_statement": "select\n         \n    id,\n    date_format(gmt_create, '%Y-%m-%d %H:%i:%s') as gmt_create,\n    date_format(gmt_modified, '%Y-%m-%d %H:%i:%s') as gmt_modified,\n    user_name,\n    login_ip,\n    method_name,\n    request_url,\n    request_params,\n    response_params,\n    order_id\n   \n        from aiit_operate_log\n        where is_delete=0 and  order_id='abcd'",
//           "db_instance": "zhejiang_mobile",
//           "serviceInstanceName": "340674ad19ed46d8af365f8ca7b74332@192.168.1.103"
//       }]
//     },
//     {
//       "url": "http://localhost:7070/user/login",
//       "segments": [{
//       "db_type": "sql",
//         "serviceCode": "demo-application-springboot",
//         "db_user_name": "root",
//         "peer": "10.0.107.46:3306",
//         "db_statement": "select\n     \n    id, is_delete, gmt_create, gmt_modified, creator, modifier, status, user_name, password,\n    salt, phone, name, email\n   \n    from sys_operator\n    where user_name='admin'",
//         "db_instance": "zhejiang_mobile",
//         "serviceInstanceName": "112c8a0cf2664a97b9e70ab3c19b7013@192.168.1.103"
//     },
//       {
//         "db_type": "sql",
//         "serviceCode": "demo-application-springboot",
//         "db_user_name": "root",
//         "peer": "10.0.107.46:3306",
//         "db_statement": "select\n         \n    id, is_delete, gmt_create, gmt_modified, user_name, password_error_count, description\n   \n        from aiit_user_login_statistics\n        where is_delete=0 AND user_name='admin'",
//         "db_instance": "zhejiang_mobile",
//         "serviceInstanceName": "112c8a0cf2664a97b9e70ab3c19b7013@192.168.1.103"
//       },
//       {
//         "db_type": "Redis",
//         "serviceCode": "demo-application-springboot",
//         "peer": "10.0.107.21:6379",
//         "db_statement": "SETEX",
//         "serviceInstanceName": "112c8a0cf2664a97b9e70ab3c19b7013@192.168.1.103"
//       },
//       {
//         "db_type": "url",
//         "serviceCode": "demo-application-springboot",
//         "peer": "localhost:8080",
//         "db_statement": "http://localhost:8080/demo1/test",
//         "serviceInstanceName": "112c8a0cf2664a97b9e70ab3c19b7013@192.168.1.103"
//       }
// 			]
//     }
// 	]
//   }
// ]
