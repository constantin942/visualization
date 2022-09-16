package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsAlarmInformationDo;
import com.mingshi.skyflying.common.domain.UserPortraitByVisitedTableEverydayDo;
import com.mingshi.skyflying.common.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.common.enums.ConstantsCode;
import com.mingshi.skyflying.common.exception.AiitException;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsAlarmInformationMapper;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTableEverydayMapper;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTimeMapper;
import com.mingshi.skyflying.service.MsAlarmInformationService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：MsAlarmInformationServiceImpl</B>
 * <B>概要说明：操作告警信息的实现类</B>
 *
 * @Author zm
 * @Date 2022年05月25日 14:05:13
 * @Param
 * @return
 **/
@Slf4j
@Service("msAlarmInformationService")
public class MsAlarmInformationServiceImpl implements MsAlarmInformationService {
    @Resource
    private MsAlarmInformationMapper msAlarmInformationMapper;
    @Resource
    private UserPortraitByVisitedTimeMapper userPortraitByVisitedTimeMapper;
    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private UserPortraitByVisitedTableEverydayMapper userPortraitByVisitedTableEverydayMapper;
    @Resource
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    @Override
    public ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName, Integer matchRuleId, String originalTime, Integer pageNo, Integer pageSize) {
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(userName)) {
            queryMap.put("userName", userName);
        }
        if (null != matchRuleId) {
            queryMap.put("matchRuleId", matchRuleId);
        }
        if (StringUtil.isNotBlank(originalTime)) {
            queryMap.put("originalTime", originalTime);
        }
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);
        List<MsAlarmInformationDo> alarmInformationDoList = msAlarmInformationMapper.selectAll(queryMap);

        Integer count = msAlarmInformationMapper.selectAllCount(queryMap);

        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", JsonUtil.obj2String(alarmInformationDoList));
        context.put("total", count);

        log.info("# MsAlarmInformationServiceImpl.getAllAlarmInfo() # 获取所有的告警信息，根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), alarmInformationDoList.size());
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(context));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> getAllAlarmInfo(String userName, Integer pageNo, Integer pageSize) {

        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(userName)) {
            queryMap.put("userName", userName);
        }
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);
        List<Map<String, Object>> alarmInformationDoList = msAlarmInformationMapper.selectAllUserTimes(queryMap);

        Integer count = msAlarmInformationMapper.selectAllUserTimesCount(queryMap);

        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", JsonUtil.obj2String(alarmInformationDoList));
        context.put("total", count);

        log.info("# MsAlarmInformationServiceImpl.getAllAlarmInfo() # 获取所有的告警信息，根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), alarmInformationDoList.size());
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(context));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> getUserNameAnomalyDetectionInfo() {
        List<String> userNameList = msAlarmInformationMapper.selectAllUserName();
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(userNameList));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> getAnomalyDetectionInfoByGroupByUserName(Integer pageNo, Integer pageSize) {
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);
        List<Map<String, Object>> alarmInformationDoList = msAlarmInformationMapper.selectAllByGroupByUserName(queryMap);

        Integer count = msAlarmInformationMapper.selectAllByGroupByUserNameCount();

        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", JsonUtil.obj2String(alarmInformationDoList));
        context.put("total", count);

        log.info("# MsAlarmInformationServiceImpl.getAllAlarmInfo() # 获取所有的告警信息，根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), alarmInformationDoList.size());
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(context));
        return bySuccess;
    }

    /**
     * <B>方法名称：updateAnomalyDetectionInfo</B>
     * <B>概要说明：处置告警异常信息</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年07月25日 13:07:17
     * @Param [userName, matchRuleId, originalTime]
     */
    public void updateAnomalyDetectionInfo(AnomalyDetectionInfoBo anomalyDetectionInfoBo) {
        if (!anomalyDetectionInfoBo.getFlag().equals(Const.ANOMALY_DETECTION_INFO_DELETE) &&
                !anomalyDetectionInfoBo.getFlag().equals(Const.ANOMALY_DETECTION_INFO_UPDATE_USER_PORTRAIT)) {
            throw new AiitException("处置字段非法，处置字段要么是delete，要么是update");
        }

        // 仅仅删除这条规则；2022-07-25 14:16:26
        if (anomalyDetectionInfoBo.getFlag().equals(Const.ANOMALY_DETECTION_INFO_DELETE)) {
            deleteAnomalyDetection(anomalyDetectionInfoBo.getId());
        } else if (anomalyDetectionInfoBo.getFlag().equals(Const.ANOMALY_DETECTION_INFO_UPDATE_USER_PORTRAIT)) {
            // 逻辑删除告警信息；
            updateAnomalyDetection(anomalyDetectionInfoBo.getId());
            // 插入粗粒度表
            anomalyDetectionBusiness.insertCoarse(anomalyDetectionInfoBo);
        }
    }

    @Override
    public ServerResponse<String> updateAnomalyDetectionInfos(List<AnomalyDetectionInfoBo> anomalyDetectionInfoBos) {
        //更新数据
        for (AnomalyDetectionInfoBo anomalyDetectionInfoBo : anomalyDetectionInfoBos) {
            updateAnomalyDetectionInfo(anomalyDetectionInfoBo);
        }
        //更新用户画像
        anomalyDetectionBusiness.updatePortrait();
        return ServerResponse.createBySuccess();
    }

    /**
     * <B>方法名称：updateAnomalyDetection</B>
     * <B>概要说明：更新用户画像信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月25日 14:07:58
     * @Param [userName, originalTime, matchRuleId]
     **/
    private void updateAnomalyDetection(AnomalyDetectionInfoBo anomalyDetectionInfoBo) {
        Integer matchRuleId = anomalyDetectionInfoBo.getMatchRuleId();
        String userName = anomalyDetectionInfoBo.getUserName();
        String originalTime = anomalyDetectionInfoBo.getOriginalTime();
        String alarmContent = anomalyDetectionInfoBo.getAlarmContent();
        if (matchRuleId.equals(1)) {
            // 处理基于访问时间的用户画像信息；2022-07-25 17:36:01
            updateOrInsertUserPortraitByVisitedTime(userName, originalTime);
        } else if (matchRuleId.equals(2)) {
            // 处理基于访问过的表的用户画像信息
            updateOrInsertUserPortraitByVisitedTable(originalTime, userName, alarmContent);
        }
    }

    /**
     * <B>方法名称：updateOrInsertUserPortraitByVisitedTable</B>
     * <B>概要说明：处理基于访问过的表的用户画像信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月25日 17:07:28
     * @Param [userName, originalTime]
     **/
    private void updateOrInsertUserPortraitByVisitedTable(String originalTime, String userName, String alarmContent) {
        // 用户 jiaojiangjg 首次访问了数据库表：hy_company。
        String[] split = alarmContent.split("：")[1].split("。");
        String[] tablesAndDbType = split[0].split("-");
        String tables = tablesAndDbType[0];
        String dbType = Const.OPERATION_TYPE_SELECT;
        if (1 < tablesAndDbType.length) {
            dbType = tablesAndDbType[1];
        }

        UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo = new UserPortraitByVisitedTableEverydayDo();
        userPortraitByVisitedTableEverydayDo.setUserName(userName);
        userPortraitByVisitedTableEverydayDo.setVisitedTable(tables);
        Date startTime = DateTimeUtil.strToDate(originalTime, DateTimeUtil.DATEFORMAT_STR_001);
        String strToDateToStr = DateTimeUtil.dateToStr(startTime, DateTimeUtil.DATEFORMAT_STR_002);

        userPortraitByVisitedTableEverydayDo.setVisitedDate(strToDateToStr);
        userPortraitByVisitedTableEverydayDo.setDbType(dbType);

        UserPortraitByVisitedTableEverydayDo userPortraitByVisitedTableEverydayDo1 = userPortraitByVisitedTableEverydayMapper.selectByUserNameAndTime(userPortraitByVisitedTableEverydayDo);
        if (null == userPortraitByVisitedTableEverydayDo1) {
            userPortraitByVisitedTableEverydayDo.setVisitedCount(1);
            userPortraitByVisitedTableEverydayMapper.insertSelective(userPortraitByVisitedTableEverydayDo);
        } else {
            Integer visitedCount = userPortraitByVisitedTableEverydayDo1.getVisitedCount();
            userPortraitByVisitedTableEverydayDo1.setVisitedCount(null == visitedCount ? 0 + 1 : visitedCount + 1);
            userPortraitByVisitedTableEverydayDo.setVisitedCount(userPortraitByVisitedTableEverydayDo1.getVisitedCount());
            userPortraitByVisitedTableEverydayMapper.updateByPrimaryKeySelective(userPortraitByVisitedTableEverydayDo1);
        }
        // 将数据更新到本地内存中；2022-07-25 17:20:44
        mingshiServerUtil.synchronizationUserPortraitByVisitedTableToLocalMemory(userPortraitByVisitedTableEverydayDo);
    }

    /**
     * <B>方法名称：updateOrInsertUserPortraitByVisitedTime</B>
     * <B>概要说明：处理基于访问时间的用户画像信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月25日 17:07:28
     * @Param [userName, originalTime]
     **/
    private void updateOrInsertUserPortraitByVisitedTime(String userName, String originalTime) {
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        queryMap.put("userName", userName);
        List<UserPortraitByVisitedTimeDo> userPortraitByVisitedTimeDoList = userPortraitByVisitedTimeMapper.selectByUserName(queryMap);
        UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo = null;
        if (null == userPortraitByVisitedTimeDoList || 0 == userPortraitByVisitedTimeDoList.size()) {
            userPortraitByVisitedTimeDo = new UserPortraitByVisitedTimeDo();
            createUserPortraitByVisitedTimeDo(userPortraitByVisitedTimeDo, originalTime);
            userPortraitByVisitedTimeMapper.insertSelective(userPortraitByVisitedTimeDo);
        } else {
            userPortraitByVisitedTimeDo = userPortraitByVisitedTimeDoList.get(0);
            createUserPortraitByVisitedTimeDo(userPortraitByVisitedTimeDo, originalTime);
            userPortraitByVisitedTimeMapper.updateByPrimaryKeySelective(userPortraitByVisitedTimeDo);
        }
        // 将数据更新到本地内存中；2022-07-25 17:20:44
        mingshiServerUtil.synchronizationUserPortraitByVisitedTimeToLocalMemory(userPortraitByVisitedTimeDo);
    }

    /**
     * <B>方法名称：createUserPortraitByVisitedTimeDo</B>
     * <B>概要说明：赋值</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月25日 17:07:06
     * @Param [userPortraitByVisitedTimeDo, originalTime]
     **/
    public void createUserPortraitByVisitedTimeDo(UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo, String originalTime) {
        Date date = DateTimeUtil.strToDate(originalTime);
        String currHourTime = DateTimeUtil.judgmentTime(date);
        if (currHourTime.equals(ConstantsCode.USER_PORTRAIT_FORENOON.getCode())) {
            Integer forenoonCount = userPortraitByVisitedTimeDo.getForenoonCount();
            userPortraitByVisitedTimeDo.setNightCount(null == forenoonCount ? 1 : forenoonCount + 1);
        } else if (currHourTime.equals(ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode())) {
            Integer afternoonCount = userPortraitByVisitedTimeDo.getAfternoonCount();
            userPortraitByVisitedTimeDo.setAfternoonCount(null == afternoonCount ? 1 : afternoonCount + 1);
        } else if (currHourTime.equals(ConstantsCode.USER_PORTRAIT_NIGHT.getCode())) {
            Integer nightCount = userPortraitByVisitedTimeDo.getNightCount();
            userPortraitByVisitedTimeDo.setNightCount(null == nightCount ? 1 : nightCount + 1);
        }
    }

    /**
     * <B>方法名称：deleteAnomalyDetection</B>
     * <B>概要说明：逻辑删除告警信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月25日 14:07:08
     * @Param [userName, originalTime, matchRuleId]
     **/
    private void deleteAnomalyDetection(Integer id) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setId(id);
        msAlarmInformationDo.setIsDelete(1);
        msAlarmInformationMapper.updateByPrimaryKeySelective(msAlarmInformationDo);
    }

    /**
     * <B>方法名称：updateAnomalyDetection</B>
     * <B>概要说明：将告警信息设置为更新画像</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年08月02日 17:08:29
     * @Param [id]
     **/
    private void updateAnomalyDetection(Integer id) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setId(id);
        msAlarmInformationDo.setUpdateUserPortrait(1);
        msAlarmInformationMapper.updateByPrimaryKeySelective(msAlarmInformationDo);
    }

    // private void deleteAnomalyDetection(String userName, String originalTime, Integer matchRuleId) {
    //   MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
    //   msAlarmInformationDo.setUserName(userName);
    //   msAlarmInformationDo.setOriginalTime(DateTimeUtil.strToDate(originalTime));
    //   if (null != matchRuleId) {
    //     msAlarmInformationDo.setMatchRuleId(matchRuleId);
    //   }
    //   msAlarmInformationDo.setIsDelete(1);
    //   msAlarmInformationMapper.updateByUserNameAndOriginalTime(msAlarmInformationDo);
    // }
}
