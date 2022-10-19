package com.mingshi.skyflying.impl;

import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogRequest;
import com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogResponse;
import com.aliyuncs.profile.DefaultProfile;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.anomaly_detection.AnomalyDetectionBusiness;
import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.dao.MsConfigDao;
import com.mingshi.skyflying.common.domain.*;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsDmsAuditLogDao;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.service.AuditLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：AuditLogServiceImpl</B>
 * <B>概要说明：将审计日志插入到数据库中</B>
 *
 * @Author zm
 * @Date 2022年05月25日 14:05:13
 * @Param
 * @return
 **/
@Slf4j
@Service("auditLogService")
public class AuditLogServiceImpl implements AuditLogService {

    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private AnomalyDetectionBusiness anomalyDetectionBusiness;
    @Resource
    private MsDmsAuditLogDao msDmsAuditLogDao;
    @Resource
    private MsScheduledTaskDao msScheduledTaskDao;
    @Resource
    private MsConfigDao msConfigDao;

    /**
     * <B>方法名称：autoFetchAuditlogByDMS</B>
     * <B>概要说明：通过定时任务，自动拉取MDS中的数据库审计日志</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年05月26日 15:05:40
     * @Param []
     **/
    @Override
    public ServerResponse<String> autoFetchAuditlogByDms(String startTime, String endTime) {
        Instant now = Instant.now();
        if (StringUtil.isBlank(startTime) || StringUtil.isBlank(endTime)) {
            log.error("#AuditLogServiceImpl.manualFetchAuditlog()# 通过定时任务自动拉取DMS审计日志，收到的开始间或者结束时间为空。");
            return ServerResponse.createByErrorMessage("开始时间或者结束时间不能为空", "");
        }
        log.info("开始执行 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS审计日志。参数 startTime = 【{}】，endTime = 【{}】。", startTime, endTime);

        // 从数据库中获取ak、sk信息；2022-10-10 09:49:55
        ObjectNode akSkObjectNode = mingshiServerUtil.getAkSk(Boolean.TRUE);
        if(null == akSkObjectNode || null == akSkObjectNode.get(Const.AK) || null == akSkObjectNode.get(Const.SK) || StringUtil.isBlank(akSkObjectNode.get(Const.AK).asText()) || StringUtil.isBlank(akSkObjectNode.get(Const.SK).asText())){
            return ServerResponse.createByErrorMessage("开始执行 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS审计日志。没有配置aksk信息", "");
        }

        // 从数据库中获取配置的数据库位置信息；2022-10-10 09:49:55
        ObjectNode dmsRegionObjectNode = getDmsRegion();
        if(null == dmsRegionObjectNode || null == dmsRegionObjectNode.get(Const.DMS_REGION) || StringUtil.isBlank(dmsRegionObjectNode.get(Const.DMS_REGION).asText())){
            return ServerResponse.createByErrorMessage("开始执行 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS审计日志。没有配置数据库所属区域（region）信息", "");
        }

        /**
         * cn-qingdao：华北1（青岛）
         * cn-beijing：华北2（北京）
         * cn-zhangjiakou：华北3（张家口）
         * cn-huhehaote：华北5（呼和浩特）
         * cn-hangzhou：华东1（杭州）
         * cn-shanghai：华东2（上海）
         * cn-shenzhen：华南1（深圳）
         * cn-chengdu：西南1（成都）
         * cn-hongkong：中国（香港）
         * ap-northeast-1：日本（东京）
         * ap-southeast-1：新加坡
         * ap-southeast-2：澳大利亚（悉尼）
         * ap-southeast-3：马来西亚（吉隆坡）
         * ap-southeast-5：印度尼西亚（雅加达）
         * us-east-1：美国（弗吉尼亚）
         * us-west-1：美国（硅谷）
         * eu-west-1：英国（伦敦）
         * eu-central-1：德国（法兰克福）
         * ap-south-1：印度（孟买）
         * me-east-1：阿联酋（迪拜）
         * cn-hangzhou-finance：华东1 金融云
         * cn-shanghai-finance-1：华东2 金融云
         * cn-shenzhen-finance-1：华南1 金融云
         * cn-beijing-finance-1：华北2 金融云
         */
        DefaultProfile profile = DefaultProfile.getProfile(dmsRegionObjectNode.get(Const.DMS_REGION).asText(), akSkObjectNode.get(Const.AK).asText(), akSkObjectNode.get(Const.SK).asText());
        IAcsClient client = new DefaultAcsClient(profile);

        ServerResponse<String> dmsAuditLog = getDmsAuditLog(client, startTime, endTime);
        log.info("执行结束 #AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志。耗时【{}】毫秒。", DateTimeUtil.getTimeMillis(now));
        return dmsAuditLog;

    }

    /**
     * <B>方法名称：getDmsRegion</B>
     * <B>概要说明：从数据库中获取配置的数据库位置信息</B>
     * @Author zm
     * @Date 2022年10月10日 09:10:59
     * @Param []
     * @return com.fasterxml.jackson.databind.node.ObjectNode
     **/
    private ObjectNode getDmsRegion() {
        ObjectNode jsonNodes = JsonUtil.createJsonObject();
        MsConfigDo msConfigDo = msConfigDao.selectByConfigType(Const.DMS_REGION);
        if (null == msConfigDo || StringUtil.isBlank(msConfigDo.getConfig())) {
            log.error("#AuditLogServiceImpl.getDmsRegion()# 通过定时任务自动拉取DMS审计日志时，在数据库中没有获取到数据库所属区域（region）配置。");
            return null;
        }
        String config = null;
        String dmsRegion = null;
        try {
            config = msConfigDo.getConfig();

            ObjectNode jsonObject = JsonUtil.string2Object(config, ObjectNode.class);
            if (null == jsonObject.get(Const.DMS_REGION) || StringUtil.isBlank(jsonObject.get(Const.DMS_REGION).asText())) {
                log.error("#AuditLogServiceImpl.getDmsRegion()# 通过定时任务自动拉取DMS审计日志时，在数据库中没有获取到数据库所属区域（region）配置。");
                return null;
            }
            dmsRegion = jsonObject.get(Const.DMS_REGION).asText();
        } catch (Exception e) {
            log.error("#AuditLogServiceImpl.getDmsRegion()# 通过定时任务自动拉取DMS的审计日志，解析在数据库中获取到的数据库所属区域（region）配置 = 【{}】时，出现了异常。", config, e);
            return null;
        }
        jsonNodes.put(Const.DMS_REGION,dmsRegion);
        return jsonNodes;
    }

    /**
     * <B>方法名称：getDMSAuditLog</B>
     * <B>概要说明：循环拉取DMS中的审计日志</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年05月26日 16:05:31
     * @Param [client, startTime, endTime]
     **/
    private ServerResponse<String> getDmsAuditLog(IAcsClient client, String startTime, String endTime) {
        ServerResponse<String> byErrorMessage = ServerResponse.createBySuccess();
        int pageSize = 100;
        int pageNumber = 0;

        int increment = 0;
        Boolean flag = true;
        while (true == flag) {
            ListSQLExecAuditLogRequest request = getRequest(startTime, endTime, pageSize, pageNumber);
            MsScheduledTaskDo msScheduledTaskDo = new MsScheduledTaskDo();

            try {
                ListSQLExecAuditLogResponse response = client.getAcsResponse(request);
                Long totalCount = response.getTotalCount();
                List<ListSQLExecAuditLogResponse.SQLExecAuditLog> sqlExecAuditLogList = response.getSQLExecAuditLogList();
                increment += sqlExecAuditLogList.size();

                if (totalCount.intValue() == increment) {
                    // 当本次拉取的审计信息小于每页设置的大小时，则说明这个时间段内的数据已经被拉取完毕了，那么就退出循环。2022-05-26 16:29:47
                    flag = false;
                }
                log.info("# AuditLogServiceImpl.autoFetchAuditlogByDMS() # 通过定时任务自动拉取DMS的审计日志，终止死循环条件是totalCount.intValue() == increment，" +
                    "当前totalCount.intValue() =【{}】，increment = 【{}】。", totalCount.intValue(), increment);

                if (!sqlExecAuditLogList.isEmpty()) {
                    // 将DMS数据插入到数据库中
                    batchProcessDmsAuditLog(sqlExecAuditLogList);
                }

                msScheduledTaskDo.setStartTime(startTime);
                msScheduledTaskDo.setEndTime(endTime);
                msScheduledTaskDo.setPageSize(pageSize);
                msScheduledTaskDo.setPageNumber(pageNumber);
                msScheduledTaskDo.setStatus(Const.RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT);
                msScheduledTaskDo.setRecordCount(sqlExecAuditLogList.size());
            } catch (Exception e) {
                msScheduledTaskDo.setStatus(Const.RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT);
                log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志时，出现了异常。", e);
                byErrorMessage = ServerResponse.createByErrorMessage("拉取DMS审计日志时，出现了异常。" + e.getMessage(), "");
            }
            // 将拉取DMS数据的记录插入到数据库中
            doInsertSelective(msScheduledTaskDo);
        }

        return byErrorMessage;
    }

    /**
     * <B>方法名称：doInsertSelective</B>
     * <B>概要说明：将拉取DMS数据的记录插入到数据库中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 17:10:03
     * @Param [msScheduledTaskDo]
     **/
    private void doInsertSelective(MsScheduledTaskDo msScheduledTaskDo) {
        try {
            if (StringUtil.isNotBlank(msScheduledTaskDo.getStartTime()) &&
                StringUtil.isNotBlank(msScheduledTaskDo.getEndTime()) &&
                null != msScheduledTaskDo.getPageNumber() &&
                null != msScheduledTaskDo.getPageSize() &&
                null != msScheduledTaskDo.getRecordCount() && 0 < msScheduledTaskDo.getRecordCount()) {
                int insertResult = msScheduledTaskDao.insertSelective(msScheduledTaskDo);
                if (1 != insertResult) {
                    log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志，把拉取到的数据 = 【{}】存入到数据库失败。", JsonUtil.obj2String(msScheduledTaskDo));
                }
            }
        } catch (Exception e) {
            log.error("#AuditLogServiceImpl.autoFetchAuditlogByDMS()# 通过定时任务自动拉取DMS的审计日志，把拉取到的数据 = 【{}】存入到数据库中出现了异常。", JsonUtil.obj2String(msScheduledTaskDo), e);
        }
    }

    /**
     * <B>方法名称：getRequest</B>
     * <B>概要说明：组装请求实例</B>
     *
     * @return com.aliyuncs.dms_enterprise.model.v20181101.ListSQLExecAuditLogRequest
     * @Author zm
     * @Date 2022年10月08日 17:10:47
     * @Param [startTime, endTime, pageSize, pageNumber]
     **/
    private ListSQLExecAuditLogRequest getRequest(String startTime, String endTime, int pageSize, int pageNumber) {
        ListSQLExecAuditLogRequest request = new ListSQLExecAuditLogRequest();
        request.setStartTime(startTime);
        request.setEndTime(endTime);
        request.setPageSize(pageSize);
        request.setPageNumber(++pageNumber);
        return request;
    }

    /**
     * <B>方法名称：batchProcessDmsAuditLog</B>
     * <B>概要说明：将DMS数据插入到数据库中</B>
     *
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年10月08日 17:10:23
     * @Param [listSqlExecAuditLogList]
     **/
    public ServerResponse<String> batchProcessDmsAuditLog(List<ListSQLExecAuditLogResponse.SQLExecAuditLog> listSqlExecAuditLogList) {
        int size = 0;
        Instant now = Instant.now();
        log.info("开始执行# AuditLogServiceImpl.batchProcessDMSAuditLog() # 将阿里云提供的DMS数据库审计日志插入到数据库中。");
        try {
            List<MsDmsAuditLogDo> list = new LinkedList<>();
            for (ListSQLExecAuditLogResponse.SQLExecAuditLog sqlExecAuditLog : listSqlExecAuditLogList) {
                String msSchemaName = sqlExecAuditLog.getSchemaName();
                if (Const.MYSQL.equals(msSchemaName) || Const.FAIL.equals(sqlExecAuditLog.getExecState())) {
                    continue;
                }
                String msSql = sqlExecAuditLog.getSQL();
                if (msSql.startsWith("show variables") || msSql.startsWith("set global") || msSql.startsWith("select @@version")) {
                    continue;
                }
                // 组装MsDmsAuditLogDo实例
                getMsDmsAuditLogDo(msSql, msSchemaName, sqlExecAuditLog, list);
            }
            size = list.size();
            if (0 < size) {
                // 将来自DMS的SQL审计日志信息存储到专门存储DMS审计日志的表中；2022-05-30 16:36:22
                msDmsAuditLogDao.insertSelectiveBatchNoSqlInsightDbUserName(list);

                // 异常检测和统计；2022-10-09 09:16:02
                anomalyDetectionStatistics(list);
            }
        } catch (Exception e) {
            log.error("将阿里云提供的DMS数据库审计日志插入到表中出现了异常。", e);
        }
        log.info("执行完毕# AuditLogServiceImpl.batchProcessDMSAuditLog() # 将阿里云提供的DMS数据库审计日志【{}条】插入到数据库中。耗时【{}】毫秒。", size, DateTimeUtil.getTimeMillis(now));
        return null;
    }

    /**
     * <B>方法名称：anomalyDetectionStatistics</B>
     * <B>概要说明：将实例MsDmsAuditLogDo转换成实例MsSegmentDetailDo，然后进行异常检测</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月09日 09:10:02
     * @Param [list]
     **/
    private void anomalyDetectionStatistics(List<MsDmsAuditLogDo> list) {
        try {
            LinkedList<MsSegmentDetailDo> segmentDetaiDolList = new LinkedList<>();
            LinkedList<MsAlarmInformationDo> msAlarmInformationDoList = new LinkedList<>();
            for (MsDmsAuditLogDo msDmsAuditLogDo : list) {
                MsSegmentDetailDo msSegmentDetailDo = new MsSegmentDetailDo();
                msSegmentDetailDo.setUserName(msDmsAuditLogDo.getUserName());
                msSegmentDetailDo.setDbUserName(msDmsAuditLogDo.getUserName());
                msSegmentDetailDo.setComponent(msDmsAuditLogDo.getSqlSource());
                msSegmentDetailDo.setStartTime(msDmsAuditLogDo.getOpTime());
                msSegmentDetailDo.setPeer(msDmsAuditLogDo.getInstanceName());
                msSegmentDetailDo.setDbType(msDmsAuditLogDo.getSqlType());
                msSegmentDetailDo.setDbStatement(msDmsAuditLogDo.getMsSql());
                msSegmentDetailDo.setMsTableName(msDmsAuditLogDo.getMsTableName());
                msSegmentDetailDo.setServiceCode(msDmsAuditLogDo.getSqlSource());
                msSegmentDetailDo.setEndTime(msDmsAuditLogDo.getOpTime());
                msSegmentDetailDo.setDbInstance(msDmsAuditLogDo.getInstanceName());
                msSegmentDetailDo.setServiceCode(msDmsAuditLogDo.getSqlSource());

                segmentDetaiDolList.add(msSegmentDetailDo);
            }
            // 将来自DMS的消息进行异常检测；2022-10-19 10:16:26
            anomalyDetectionBusiness.userVisitedIsAbnormal(segmentDetaiDolList, msAlarmInformationDoList);
            if(Boolean.TRUE.equals(MsCaffeineCache.getUserPortraitInitDone())){
                // 当用户画像初始化成功了，才将来自DMS中的数据保存到数据库中；2022-10-19 10:31:52
                mingshiServerUtil.flushSegmentDetailToDb(segmentDetaiDolList);
                mingshiServerUtil.flushAbnormalToDb(msAlarmInformationDoList);
            }
        } catch (Exception e) {
            log.error("# AuditLogServiceImpl.anomalyDetectionStatistics() # 将DMS中的数据库审计日志转换成来自探针的数据库SQL语句，并进行异常检测和统计时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getMsDmsAuditLogDo</B>
     * <B>概要说明：组装MsDmsAuditLogDo实例</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年10月08日 17:10:57
     * @Param [msSql, msSchemaName, sqlExecAuditLog, list]
     **/
    private void getMsDmsAuditLogDo(String msSql, String msSchemaName, ListSQLExecAuditLogResponse.SQLExecAuditLog sqlExecAuditLog, List<MsDmsAuditLogDo> list) {
        // 大写统一转换成消息、去掉多余的空格
        String strData = StringUtil.recombination(msSql, sqlExecAuditLog.getOpTime(), msSchemaName, sqlExecAuditLog.getSQLType());
        String hash = StringUtil.mD5(strData);

        MsDmsAuditLogDo msDmsAuditLogDo = new MsDmsAuditLogDo();

        String sqlType1 = mingshiServerUtil.getSqlType(msSql);
        if (StringUtil.isBlank(sqlType1)) {
            return;
        }
        // 获取表名；2022-06-06 14:11:21
        try {
            if (!sqlType1.equals(Const.SQL_TYPE_NONE.trim()) && StringUtil.isNotBlank(sqlType1) && !Const.FAIL.equals(sqlExecAuditLog.getExecState())) {
                String tableName = mingshiServerUtil.getTableName(sqlType1, msSql);
                msDmsAuditLogDo.setMsTableName(tableName);
            }
        } catch (Exception e) {
            log.error("# AuditLogServiceImpl.getMsDmsAuditLogDo() # 根据sql语句 = 【{}】获取表名时，出现了异常。", msSql, e);
        }

        msDmsAuditLogDo.setMsSql(msSql);
        msDmsAuditLogDo.setSqlSource(Const.SQL_SOURCE_DMS);
        msDmsAuditLogDo.setMsSchemaName(msSchemaName);
        msDmsAuditLogDo.setOpTime(sqlExecAuditLog.getOpTime());
        msDmsAuditLogDo.setUserName(sqlExecAuditLog.getUserName());
        msDmsAuditLogDo.setUserId(sqlExecAuditLog.getUserId());
        msDmsAuditLogDo.setInstanceName(sqlExecAuditLog.getInstanceName());
        msDmsAuditLogDo.setInstanceId(sqlExecAuditLog.getInstanceId());
        msDmsAuditLogDo.setDbId(sqlExecAuditLog.getDbId());
        msDmsAuditLogDo.setLogic(sqlExecAuditLog.getLogic());
        msDmsAuditLogDo.setSqlType(sqlExecAuditLog.getSQLType());
        msDmsAuditLogDo.setExecState(sqlExecAuditLog.getExecState());
        msDmsAuditLogDo.setAffectRows(sqlExecAuditLog.getAffectRows());
        msDmsAuditLogDo.setElapsedTime(sqlExecAuditLog.getElapsedTime());
        msDmsAuditLogDo.setRemark(sqlExecAuditLog.getRemark());
        msDmsAuditLogDo.setHash(hash);

        list.add(msDmsAuditLogDo);
    }

    /**
     * <B>方法名称：getDmsAuditLogFromDb</B>
     * <B>概要说明：从数据库中获取来自DMS的数据库审计日志</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年06月15日 15:06:35
     * @Param []
     **/
    @Override
    public ServerResponse<String> getDmsAuditLogFromDb(String dbUserName,
                                                       String sqlType, /* SQL语句的类型；是insert、select、update、delete等 */
                                                       String msTableName, /* 数据库表名 */
                                                       String startTime, /* 开始时间 */
                                                       String endTime, /* 结束时间 */
                                                       Integer pageNo,
                                                       Integer pageSize) {
        Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
        if (StringUtil.isNotBlank(sqlType)) {
            queryMap.put("sqlType", sqlType);
        }
        if (StringUtil.isNotBlank(msTableName)) {
            queryMap.put("msTableName", msTableName);
        }
        if (StringUtil.isNotBlank(startTime)) {
            queryMap.put(Const.START_TIME, startTime);
        }
        if (StringUtil.isNotBlank(endTime)) {
            queryMap.put(Const.END_TIME, endTime);
        }
        if (StringUtil.isNotBlank(dbUserName)) {
            queryMap.put("userName", dbUserName);
        }
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);

        List<MsDmsAuditLogDo> listMsAuditLog = msDmsAuditLogDao.selectAll(queryMap);
        Integer count = msDmsAuditLogDao.selectAllCount(queryMap);
        Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
        context.put("rows", JsonUtil.obj2String(listMsAuditLog));
        context.put("total", count);
        log.info("执行完毕 AuditLogServiceImpl.getDmsAuditLogFromDb() # 获取来自DMS的数据库审计日志信息。");
        return ServerResponse.createBySuccess(Const.SUCCESS_MSG, Const.SUCCESS, JsonUtil.obj2String(context));
    }

    @Override
    public ServerResponse<String> getAllUserNameFromDms() {
        List<String> userNameList = msDmsAuditLogDao.selectAllUserName();
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(userNameList));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> getAllSqlTypeFromDms() {
        List<String> list = msDmsAuditLogDao.selectAllSqlType();
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(list));
        return bySuccess;
    }

    @Override
    public ServerResponse<String> getAllTableNameFromDms() {
        List<String> list = msDmsAuditLogDao.selectAllTableName();
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(JsonUtil.obj2String(list));
        return bySuccess;
    }


}
