package com.mingshi.skyflying.anomaly_detection;

import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTableEveryday;
import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.domain.SegmentDo;
import com.mingshi.skyflying.enums.ConstantsCode;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <B>主类名称: AnomalyDetectionUtil</B>
 * <B>概要说明：异常检测的工具类</B>
 * Author zm
 * Date 2022/6/9 17:16
 *
 * @Version 1.0
 **/
@Slf4j
public class AnomalyDetectionUtil {
  /**
   * <B>方法名称：userVisitedTimeIsAbnormal</B>
   * <B>概要说明：判断用户的访问时间是否异常</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 17:06:37
   * @Param [segmentDo, list]
   **/
  public static void userVisitedTimeIsAbnormal(SegmentDo segmentDo, LinkedList<MsAlarmInformationDo> list) {
    Map<String, Map<String, Integer>> userPortraitByVisitedTimeMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
    if (null == userPortraitByVisitedTimeMap) {
      // 判断用户的访问时间是否异常；2022-06-08 17:56:17
      return;
    }
    String userName = segmentDo.getUserName();
    String startTimeStr = segmentDo.getRequestStartTime();
    String globalTraceId = segmentDo.getGlobalTraceId();
    if (StringUtil.isBlank(userName) || StringUtil.isBlank(startTimeStr) || StringUtil.isBlank(globalTraceId)) {
      return;
    }
    // TODO：设置kafka保存消息的时间为1个月；2022-06-08 11:26:21
    // 标识该条记录已进行过基于访问时间的异常检测；2022-06-08 09:02:37
    segmentDo.setUserPortraitFlagByVisitedTime(1);
    Date date = DateTimeUtil.strToDate(startTimeStr);
    String currHourTime = DateTimeUtil.judgmentTime(date);
    Map<String, Integer> map = userPortraitByVisitedTimeMap.get(userName);
    MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
    Integer count = null;
    if (null == map) {
      log.error("# LoadUserPortraitFromDb.urlIsAbnormal() # 出现了一条告警信息。该用户从来没有出现过。", userName);
      msAlarmInformationDo.setUserName(userName);
      msAlarmInformationDo.setGlobalTraceId(globalTraceId);
      msAlarmInformationDo.setAlarmContent("用户[" + userName + "]在之前从来没有出现过。");
      list.add(msAlarmInformationDo);
    } else {
      String content = null;
      if (currHourTime.equals(ConstantsCode.USER_PORTRAIT_FORENOON.getMsgEn())) {
        count = map.get(ConstantsCode.USER_PORTRAIT_FORENOON.getMsgEn());
        map.put(ConstantsCode.USER_PORTRAIT_FORENOON.getMsgEn(), null == count ? (null) : (1 + count));
        content = ConstantsCode.USER_PORTRAIT_FORENOON.getMsgCn();
      } else if (currHourTime.equals(ConstantsCode.USER_PORTRAIT_AFTERNOON.getMsgEn())) {
        count = map.get(ConstantsCode.USER_PORTRAIT_AFTERNOON.getMsgEn());
        map.put(ConstantsCode.USER_PORTRAIT_AFTERNOON.getMsgEn(), null == count ? (null) : (1 + count));
        content = ConstantsCode.USER_PORTRAIT_AFTERNOON.getMsgCn();
      } else if (currHourTime.equals(ConstantsCode.USER_PORTRAIT_NIGHT.getMsgEn())) {
        count = map.get(ConstantsCode.USER_PORTRAIT_NIGHT.getMsgEn());
        map.put(ConstantsCode.USER_PORTRAIT_NIGHT.getMsgEn(), null == count ? (null) : (1 + count));
        content = ConstantsCode.USER_PORTRAIT_NIGHT.getMsgCn();
      } else {
        log.error("# LoadUserPortraitFromDb.urlIsAbnormal() # 用户【{}】访问时间【{}】既不是上午、下午，也不是晚上。这是不对的，要排查下原因。", userName, startTimeStr);
      }
      if (null == count || 0 == count) {
        msAlarmInformationDo.setUserName(userName);
        msAlarmInformationDo.setGlobalTraceId(globalTraceId);
        msAlarmInformationDo.setAlarmContent("用户 " + userName + " 在之前从来没有在 " + content + " 访问过系统。");
        list.add(msAlarmInformationDo);
      } else {
        // 设置变更标记；2022-06-08 10:53:05
        AnomylyDetectionSingletonByVisitedTime.setUserPortraitByVisitedTimeIsChanged(true);
      }
    }
  }

  /**
   * <B>方法名称：userVisitedTableIsAbnormal</B>
   * <B>概要说明：判断用户的访问的表是否异常</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月08日 17:06:37
   * @Param [segmentDo, list]
   **/
  public static void userVisitedTableIsAbnormal(LinkedList<MsAlarmInformationDo> list, List<MsSegmentDetailDo> segmentDetaiDolList) {
    Map<String/* 用户名 */, Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */,Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>>> userPortraitByVisitedTableEverydayMap =
      AnomylyDetectionSingletonByVisitedTableEveryday.getUserPortraitByVisitedTableMap();
    if (null == userPortraitByVisitedTableEverydayMap || 0 == userPortraitByVisitedTableEverydayMap.size() || null == segmentDetaiDolList || 0 == segmentDetaiDolList.size()) {
      // 判断用户的访问过的表是否异常；2022-06-08 17:56:17
      return;
    }
    for (MsSegmentDetailDo msSegmentDetailDo : segmentDetaiDolList) {
      String dbType = msSegmentDetailDo.getDbType();
      String operationType = msSegmentDetailDo.getOperationType();
      String tableName = msSegmentDetailDo.getMsTableName();
      String userName = msSegmentDetailDo.getUserName();
      String globalTraceId = msSegmentDetailDo.getGlobalTraceId();
      String startTime = msSegmentDetailDo.getStartTime();
      Date date = DateTimeUtil.strToDate(startTime, DateTimeUtil.DATEFORMAT_STR_001);
      String strToDateToStr = DateTimeUtil.dateToStr(date, DateTimeUtil.DATEFORMAT_STR_002);

      if (StringUtil.isNotBlank(operationType) && operationType.equals(Const.SQL) && StringUtil.isNotBlank(userName) && StringUtil.isNotBlank(tableName) && StringUtil.isNotBlank(globalTraceId)) {
        // 标识该条记录已进行过基于访问过的表的异常检测；2022-06-08 09:02:37
        msSegmentDetailDo.setUserPortraitFlagByVisitedTableEveryday(1);
        Map<String/* 访问过的表 */, Map<String/* 访问日期，以天为单位 */,Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>>> visitedTableDateCountMap = userPortraitByVisitedTableEverydayMap.get(userName);
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        Integer count = null;
        if (null == visitedTableDateCountMap || null == visitedTableDateCountMap.get(tableName)) {
          log.error("# LoadUserPortraitFromDb.userVisitedTableIsAbnormal() # 出现了一条告警信息。该用户从来没有出现过。", userName);
          msAlarmInformationDo.setUserName(userName);
          msAlarmInformationDo.setGlobalTraceId(globalTraceId);
          msAlarmInformationDo.setAlarmContent("用户[" + userName + "]在之前从来没有访问过这个表 " + tableName + "。");
          list.add(msAlarmInformationDo);
        } else {
          Map<String/* 访问日期，以天为单位 */,Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */>> dateCountMap = visitedTableDateCountMap.get(tableName);

          Map<String,/* 数据库操作类型：insert、delete、update、select */ Integer/* 访问次数 */> dbTypeCountMap = dateCountMap.get(strToDateToStr);
          try {
            if(StringUtil.isNotBlank(dbType)){
              count = dbTypeCountMap.get(dbType);
            }
            if(null == dbTypeCountMap){
              dbTypeCountMap = new ConcurrentHashMap<>();
              dateCountMap.put(strToDateToStr,dbTypeCountMap);
            }
            dbTypeCountMap.put(strToDateToStr, null == count ? 1 : (1 + count));
          } catch (Exception e) {
            e.printStackTrace();
          }

          // 设置变更标记；2022-06-08 10:53:05
          AnomylyDetectionSingletonByVisitedTableEveryday.setUserPortraitByVisitedTableIsChanged(true);
        }
      }
    }
  }
}
