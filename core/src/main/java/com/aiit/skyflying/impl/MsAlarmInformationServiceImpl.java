package com.aiit.skyflying.impl;

import com.aiit.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.aiit.skyflying.common.bo.AnomalyDetectionInfoBo;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.MsAlarmInformationDo;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.utils.JsonUtil;
import com.aiit.skyflying.common.utils.StringUtil;
import com.aiit.skyflying.common.dao.MsAlarmInformationMapper;
import com.aiit.skyflying.service.MsAlarmInformationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestBody;

import javax.annotation.Resource;
import java.util.*;

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
    AnomalyDetectionBusiness anomalyDetectionBusiness;

    @Resource
    MsAlarmInformationService alarmInformationService;


    @Override
    public ServerResponse<String> getAllAlarmInfoDetailByUserName(String userName, Integer matchRuleId, String originalTime, Integer pageNo, Integer pageSize) {
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(userName)) {
            queryMap.put(Const.USER_NAME, userName);
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
        context.put(Const.ROWS, JsonUtil.obj2String(alarmInformationDoList));
        context.put(Const.TOTAL, count);

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
     * @return com.aiit.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年07月25日 13:07:17
     * @Param [userName, matchRuleId, originalTime]
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void updateAnomalyDetectionInfo(AnomalyDetectionInfoBo anomalyDetectionInfoBo) {
        // 逻辑删除告警信息；
        alarmInformationService.deleteAnomalyDetection(anomalyDetectionInfoBo.getId());
        if (Const.ANOMALY_DETECTION_INFO_UPDATE_USER_PORTRAIT.equals(anomalyDetectionInfoBo.getUpdateUserPortrait())) {
            // 插入粗粒度表
            anomalyDetectionBusiness.insertCoarse(anomalyDetectionInfoBo);
        }
    }

    @Override
    public void updateAnomalyDetectionInfos(@RequestBody List<AnomalyDetectionInfoBo> anomalyDetectionInfoBos) {
        //更新数据
        for (AnomalyDetectionInfoBo anomalyDetectionInfoBo : anomalyDetectionInfoBos) {
            alarmInformationService.updateAnomalyDetectionInfo(anomalyDetectionInfoBo);
        }
        //更新画像
        if (Objects.equals(anomalyDetectionInfoBos.get(0).getUpdateUserPortrait(), Const.ANOMALY_DETECTION_INFO_UPDATE_USER_PORTRAIT)) {
            anomalyDetectionBusiness.updatePortrait();
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
    @Override
    public void deleteAnomalyDetection(Integer id) {
        MsAlarmInformationDo msAlarmInformationDo = new MsAlarmInformationDo();
        msAlarmInformationDo.setId(id);
        msAlarmInformationDo.setIsDelete(1);
        msAlarmInformationMapper.updateByPrimaryKeySelective(msAlarmInformationDo);
    }

}
