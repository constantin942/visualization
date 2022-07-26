package com.mingshi.skyflying.elasticsearch.utils;

import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * <B>主类名称: EsMsSegmentDetailUtil</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/7/13 08:06
 *
 * @Version 1.0
 **/
@Slf4j
@Component
@PropertySource("classpath:application-${spring.profiles.active}.yml")
public class EsMsSegmentDetailUtil {
  // es启用状态；2022-07-13 08:22:04
  @Value("${es.enable}")
  private boolean esEnable;

  public boolean getEsEnable(){
    return esEnable;
  }

  @Resource
  private MingshiServerUtil mingshiServerUtil;

  // public LinkedList<EsMsSegmentDetailDo> getEsSegmentDetaiDolList(SegmentDo segment) {
  //   LinkedList<EsMsSegmentDetailDo> esSegmentDetaiDolList = null;
  //   try {
  //     esSegmentDetaiDolList = new LinkedList<>();
  //     String reorganizingSpans = segment.getReorganizingSpans();
  //     if (StringUtil.isBlank(reorganizingSpans)) {
  //       return esSegmentDetaiDolList;
  //     }
  //     List<LinkedHashMap> list = JsonUtil.string2Obj(reorganizingSpans, List.class, LinkedHashMap.class);
  //     if (null == list || 0 == list.size()) {
  //       return esSegmentDetaiDolList;
  //     }
  //     LinkedHashMap map1 = list.get(0);
  //     Object url = map1.get("url");
  //     EsMsSegmentDetailDo esMsSegmentDetailDo = null;
  //     for (int i = 1; i < list.size(); i++) {
  //       esMsSegmentDetailDo = new EsMsSegmentDetailDo();
  //       esMsSegmentDetailDo.setUserPortraitFlagByVisitedTime(null == segment.getUserPortraitFlagByVisitedTime() ? 0 : segment.getUserPortraitFlagByVisitedTime());
  //       LinkedHashMap map = list.get(i);
  //       esMsSegmentDetailDo.setOperationName(String.valueOf(url));
  //
  //       Integer spanId = null;
  //       if (null != map.get("spanId")) {
  //         spanId = Integer.valueOf(String.valueOf(map.get("spanId")));
  //       }
  //
  //       String component = String.valueOf(map.get("component"));
  //       String serviceCode = String.valueOf(map.get("serviceCode"));
  //       String peer = String.valueOf(map.get("peer"));
  //       String endpointName = String.valueOf(map.get("endpointName"));
  //       Long startTime = Long.valueOf(String.valueOf(map.get("startTime")));
  //       String serviceInstanceName = String.valueOf(map.get("serviceInstanceName"));
  //       Long endTime = Long.valueOf(String.valueOf(map.get("endTime")));
  //       Integer parentSpanId = Integer.valueOf(String.valueOf(map.get("parentSpanId")));
  //       String tags = String.valueOf(map.get("tags"));
  //       String logs = String.valueOf(map.get("logs"));
  //
  //       Boolean isError = false;
  //
  //       List<KeyValue> tagsList = JsonUtil.string2Obj(tags, List.class, KeyValue.class);
  //       if (null != tagsList) {
  //         String isSql = null;
  //         for (KeyValue keyValue : tagsList) {
  //           String key = keyValue.getKey();
  //           String value = keyValue.getValue();
  //           if (key.equals("db.type")) {
  //             isSql = value;
  //             esMsSegmentDetailDo.setOperationType(value);
  //           } else if (key.equals("db.instance")) {
  //             esMsSegmentDetailDo.setDbInstance(value);
  //           } else if (key.equals("db_user_name")) {
  //             esMsSegmentDetailDo.setDbUserName(value);
  //           } else if (key.equals("db.statement")) {
  //             if (isSql.equals("sql")) {
  //               if ((StringUtil.isNotBlank(logs) && !logs.equals("null")) || StringUtil.isBlank(value)) {
  //                 isError = true;
  //                 // 出现了SQL异常，直接退出循环；2022-07-01 14:41:50
  //                 break;
  //               }
  //               // 获取表名；2022-06-06 14:16:59
  //               String tableName = esSetTableName(value, esMsSegmentDetailDo);
  //               if (StringUtil.isBlank(tableName)) {
  //                 isError = true;
  //                 // 出现了SQL异常，直接退出循环；2022-07-01 14:41:50
  //                 break;
  //               }
  //               esMsSegmentDetailDo.setMsTableName(tableName);
  //             }
  //             esMsSegmentDetailDo.setDbStatement(value);
  //           } else if (key.equals("url")) {
  //             esMsSegmentDetailDo.setDbType("url");
  //             esMsSegmentDetailDo.setDbStatement(value);
  //           }
  //         }
  //       }
  //
  //       if (false == isError) {
  //         // 当没有出现sql异常时，才保存SQL信息；2022-07-01 14:41:31
  //         esMsSegmentDetailDo.setToken(segment.getToken());
  //         esMsSegmentDetailDo.setComponent(component);
  //         esMsSegmentDetailDo.setSpanId(spanId);
  //         esMsSegmentDetailDo.setServiceCode(serviceCode);
  //         esMsSegmentDetailDo.setPeer(peer);
  //         esMsSegmentDetailDo.setEndpointName(endpointName);
  //         esMsSegmentDetailDo.setStartTime(DateTimeUtil.strToDate(DateTimeUtil.longToDate(startTime)));
  //         esMsSegmentDetailDo.setGmtCreate(new Date());
  //         esMsSegmentDetailDo.setGmtModified(new Date());
  //         esMsSegmentDetailDo.setServiceInstanceName(serviceInstanceName);
  //         esMsSegmentDetailDo.setEndTime(DateTimeUtil.strToDate(DateTimeUtil.longToDate(endTime)));
  //         esMsSegmentDetailDo.setParentSpanId(parentSpanId);
  //         esMsSegmentDetailDo.setUserName(segment.getUserName());
  //         esMsSegmentDetailDo.setGlobalTraceId(segment.getGlobalTraceId());
  //         esMsSegmentDetailDo.setParentSegmentId(segment.getParentSegmentId());
  //         esMsSegmentDetailDo.setCurrentSegmentId(segment.getCurrentSegmentId());
  //         esSegmentDetaiDolList.add(esMsSegmentDetailDo);
  //       }
  //     }
  //   } catch (Exception e) {
  //     log.error("# SegmentConsumeServiceImpl.getSegmentDetaiDolList() # 组装segmentDetail详情实例时，出现了异常。", e);
  //   }
  //   return esSegmentDetaiDolList;
  // }
  //
  // /**
  //  * <B>方法名称：setTableName</B>
  //  * <B>概要说明：获取表的名字</B>
  //  * @Author zm
  //  * @Date 2022年07月13日 08:07:01
  //  * @Param [value, msSegmentDetailDo]
  //  * @return void
  //  **/
  // private void setTableName(String value, MsSegmentDetailDo msSegmentDetailDo) {
  //   try {
  //     // sql类型；
  //     String sqlType = mingshiServerUtil.getSqlType(value);
  //     msSegmentDetailDo.setDbType(sqlType);
  //     // 获取表名；2022-06-06 14:11:21
  //     String tableName = mingshiServerUtil.getTableName(sqlType, value);
  //     msSegmentDetailDo.setMsTableName(tableName);
  //   } catch (Exception e) {
  //     log.error("# SegmentConsumeServiceImpl.setTableName() # 根据sql语句获取表名时，出现了异常。", e);
  //   }
  // }
  //
  // /**
  //  * <B>方法名称：setTableName</B>
  //  * <B>概要说明： 根据sql语句获取表名</B>
  //  *
  //  * @return void
  //  * @Author zm
  //  * @Date 2022年06月06日 14:06:09
  //  * @Param [value, msSegmentDetailDo]
  //  **/
  // private String esSetTableName(String value, EsMsSegmentDetailDo msSegmentDetailDo) {
  //   String tableName = null;
  //   try {
  //     // sql类型；
  //     String sqlType = mingshiServerUtil.getSqlType(value);
  //     msSegmentDetailDo.setDbType(sqlType);
  //     // 获取表名；2022-06-06 14:11:21
  //     tableName = mingshiServerUtil.getTableName(sqlType, value);
  //   } catch (Exception e) {
  //     log.error("# SegmentConsumeServiceImpl.setTableName() # 根据sql语句获取表名时，出现了异常。", e);
  //   }
  //   return tableName;
  // }

}
