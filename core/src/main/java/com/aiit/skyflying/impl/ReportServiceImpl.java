package com.aiit.skyflying.impl;

import com.aiit.skyflying.common.utils.*;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.dao.MsAgentInformationMapper;
import com.aiit.skyflying.common.dao.MsSystemOperationRecordMapper;
import com.aiit.skyflying.common.dao.MsUserFromMapper;
import com.aiit.skyflying.common.domain.*;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.service.ReportService;
import com.aiit.skyflying.service.UserFromService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Service;
import scala.App;

import javax.annotation.Resource;
import java.time.Duration;
import java.time.Instant;
import java.util.*;

/**
 * <B>类名称：ReportServiceImpl</B>
 * <B>概要说明：生成报告、获取报告</B>
 *
 * @Author zm
 * @Date 2022-12-01 11:17:13
 * @Param
 * @return
 **/
@Slf4j
@Service
public class ReportServiceImpl extends BaseParentServiceImpl<MsReport, Long> implements ReportService {
    @Resource
    private MingshiServerUtil mingshiServerUtil;
    @Resource
    private RedisPoolUtil redisPoolUtil;
    @Resource
    private MsAgentInformationMapper msAgentInformationMapper;
    @Resource
    private MsSystemOperationRecordMapper msSystemOperationRecordMapper;

    @Resource
    private SegmentDetailServiceImpl segmentDetailService;

    @Override
    public ServerResponse<String> generateReport() {
        ObjectNode returnAllJsonObject = JsonUtil.createJsonObject();
        // 1. 获取可视化系统的运行记录信息
        getAgentServerOperationRecord(returnAllJsonObject);

        // 2. 单个业务应用系统
        getSingleApplicationRecord(returnAllJsonObject);

        // 3. DMS（SQL审计）
        getDmsRecord(returnAllJsonObject);

        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        bySuccess.setData(returnAllJsonObject.toString());
        return bySuccess;
    }

    /**
     * <B>方法名称：getDmsRecord</B>
     * <B>概要说明：DMS（SQL审计）</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 10:01:13
     * @Param [returnAllJsonObject]
     **/
    private void getDmsRecord(ObjectNode returnAllJsonObject) {
        /**
         * 3. DMS（SQL审计）
         * 1）告警次数：告警分布和遗留问题记录；
         * 2）相关数据库表清单和次数；
         */

    }

    /**
     * <B>方法名称：getSingleApplicationRecord</B>
     * <B>概要说明：单个业务应用系统</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 10:00:24
     * @Param []
     **/
    private void getSingleApplicationRecord(ObjectNode returnAllJsonObject) {
        ObjectNode jsonObject = JsonUtil.createJsonObject();
        /**
         * 2. 单个业务应用系统
         * 1）运行时长；
         * 2）用户数量：总量与新增；
         * 3）访问行为数：即SQL的类型，比如insert、select、update、delete等；
         * 4）相关数据库表清单和次数；
         * 5）调用数据接口的清单和次数；
         * 6）告警次数：告警分布和遗留问题记录；
         * 7）同比上期的结果；
         * 8）历史各次总结的走势；
         */
        // 1）运行时长
        getSingleRegulatedApplicationRunTime(jsonObject);

        // 2）用户数量：总量与新增
        getSingleRegulatedApplicationNumberOfUsers(jsonObject);

        // 3）访问行为数
        getSingleRegulatedApplicationNumberOfAccessTypeTimes(jsonObject);

        // 4）相关数据库表清单和访问次数
        getSingleRegulatedApplicationTableListAndAccessTimes(jsonObject);

        // 5）调用数据接口的清单和次数
        getSingleRegulatedApplicationInterfaceListAndAccessTimes(jsonObject);

        // 6）告警次数：告警分布和遗留问题记录

        // 7）同比上期的结果

        // 8）历史各次总结的走势

        returnAllJsonObject.set(Const.REPORT_SINGLE_APPLICATION_RUN_RECORD, jsonObject);
    }

    /**
     * <B>方法名称：getSingleRegulatedApplicationInterfaceListAndAccessTimes</B>
     * <B>概要说明：单个应用系统访问接口的清单和次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-07 09:54:35
     * @Param [jsonObject]
     **/
    private void getSingleRegulatedApplicationInterfaceListAndAccessTimes(ObjectNode jsonObject) {
    }

    /**
     * <B>方法名称：getSingleRegulatedApplicationTableListAndAccessTimes</B>
     * <B>概要说明：单个应用系统表清单和访问次数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-07 09:53:51
     * @Param [jsonObject]
     **/
    private void getSingleRegulatedApplicationTableListAndAccessTimes(ObjectNode jsonObject) {
    }

    /**
     * <B>方法名称：getSingleRegulatedApplicationNumberOfAccessTypeTimes</B>
     * <B>概要说明：当个应用系统洪湖访问行为数</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-07 09:53:20
     * @Param [jsonObject]
     **/
    private void getSingleRegulatedApplicationNumberOfAccessTypeTimes(ObjectNode jsonObject) {
        try {
            // 1. 获取有多少个业务系统
            Set<String> serviceCodeSet = redisPoolUtil.reverseRange(Const.REPORT_REGULATED_ALL_OF_APPLICATION, 0L, 0L);
            ObjectNode accessTypeTimesJsonObject = JsonUtil.createJsonObject();
            // 2. 获取每个业务系统中有多少用户；
            for (String serviceCode : serviceCodeSet) {
                ArrayNode jsonArray = JsonUtil.createJsonArray();
                Set<String> userSet = redisPoolUtil.reverseRange(Const.REPORT_REGULATED_ALL_OF_APPLICATION + Const.POUND_KEY + serviceCode, 0L, 0L);
                for (String userName : userSet) {
                    String key = Const.REPORT_SINGLE_REGULATED_APPLICATION_USER_ACCESS_TYPE_TIMES + Const.POUND_KEY + serviceCode + Const.POUND_KEY + userName;
                    // 从有序集合zset中获取对每个表操作类型统计；2022-12-07 17:30:17
                    Set<ZSetOperations.TypedTuple<String>> set = redisPoolUtil.reverseRangeWithScores(key, 0L, 100000 * 10000L);
                    if (null != set && !set.isEmpty()) {
                        Iterator<ZSetOperations.TypedTuple<String>> iterator = set.iterator();
                        while (iterator.hasNext()) {
                            ZSetOperations.TypedTuple<String> next = iterator.next();
                            Double score = next.getScore();
                            String dbType = next.getValue();
                            ObjectNode userSizeJsonObject = JsonUtil.createJsonObject();
                            userSizeJsonObject.put(Const.USER_NAME, userName);
                            userSizeJsonObject.put(Const.ACCESS_TYPE, dbType);
                            userSizeJsonObject.put(Const.ACCESS_TYPE_TIMES, score);
                            jsonArray.add(userSizeJsonObject);
                            // todo：将当前系统中每个用户的访问次数插入到数据库中，以便下次统计新增；2022-12-07 16:29:16

                        }
                    }
                }
                accessTypeTimesJsonObject.put(Const.SERVICE_CODE, serviceCode);
                accessTypeTimesJsonObject.set(Const.USER_ACCESS_TYPE_TIMES, jsonArray);
            }
            ObjectNode returnJson = JsonUtil.createJsonObject();
            returnJson.put(Const.REPORT_DESC,Const.REPORT_SINGLE_REGULATED_APPLICATION_USER_ACCESS_TYPE_TIMES_DESC);
            returnJson.set(Const.REPORT_SINGLE_REGULATED_APPLICATION_USER_ACCESS_TYPE_TIMES,accessTypeTimesJsonObject);

            jsonObject.put(Const.REPORT_SINGLE_REGULATED_APPLICATION_USER_ACCESS_TYPE_TIMES_NAME, returnJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getSingleRegulatedApplicationNumberOfAccessTypeTimes() # 功能【在单个系统中，获取用户总量与新增】出现了异常。", e);
        }

    }

    /**
     * <B>方法名称：getSingleRegulatedApplicationNumberOfUsers</B>
     * <B>概要说明：在单个系统中，获取用户总量与新增</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-07 09:48:24
     * @Param [jsonObject]
     **/
    private void getSingleRegulatedApplicationNumberOfUsers(ObjectNode jsonObject) {
        try {
            // 1. 获取有多少个业务系统
            Set<String> applicationSet = redisPoolUtil.reverseRange(Const.REPORT_REGULATED_ALL_OF_APPLICATION, 0L, 0L);
            LinkedList<ObjectNode> list = new LinkedList<>();
            // 2. 获取每个业务系统中用户的访问次数；
            for (String application : applicationSet) {
                Long userSize = redisPoolUtil.sizeFromZset(Const.REPORT_REGULATED_ALL_OF_APPLICATION + Const.POUND_KEY + application);
                ObjectNode userSizeJsonObject = JsonUtil.createJsonObject();
                userSizeJsonObject.put(Const.SERVICE_CODE, application);
                userSizeJsonObject.put(Const.EVERY_USER_ACCESS_TYPE_TIMES, userSize);
                list.add(userSizeJsonObject);
                // todo：将当前系统中每个用户的访问次数插入到数据库中，以便下次统计新增；2022-12-07 16:29:16

            }
            ObjectNode returnJson = JsonUtil.createJsonObject();
            returnJson.put(Const.REPORT_DESC, Const.REPORT_SINGLE_REGULATED_APPLICATION_NUMBER_OF_USERS_DESC);
            returnJson.put(Const.REPORT_SINGLE_REGULATED_APPLICATION_NUMBER_OF_USERS, JsonUtil.obj2String(list));
            jsonObject.set(Const.REPORT_SINGLE_REGULATED_APPLICATION_NUMBER_OF_USERS_NAME, returnJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getSingleRegulatedApplicationNumberOfUsers() # 功能【在单个系统中，获取用户总量与新增】出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getAgentServerOperationRecord</B>
     * <B>概要说明：获取可视化系统的运行记录信息</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 09:54:10
     * @Param [returnAllJsonObject]
     **/
    private void getAgentServerOperationRecord(ObjectNode returnAllJsonObject) {
        ObjectNode jsonObject = JsonUtil.createJsonObject();
        /**
         * 1. 可视化系统运行记录
         * 1）可视化系统运行时长
         * 2）采集数据访问行为数
         * 3）受监管的数据库表个数：总数与新增
         * 4）产生告警次数：告警分布与告警处置的记录
         * 5）受监管的应用清单：总量与新增
         * 6）受监管的用户数量：总量与新增
         */
        // 1）可视化系统运行时长
        getReportAgentServerName(jsonObject);

        /**
         * 具体实现步骤：
         * 1）在数据库中建立一个数据库表：system_operation_record，专门用于存放可视化系统和各个业务系统的运行记录信息，其中有一个字段是系统开始运行时间，一个字段是系统在运行期间心跳更新时间；
         * 2）当可视化系统或者业务系统启动时，先检查下当前系统信息在数据库表system_operation_record中是否存在，如果不存在，则插入，如果存在，则更新。可以在插入时，更新心跳信息；
         * 3）
         */

        // 2） 采集数据访问行为数
        getDataAccessBehavior(jsonObject);

        // 3）受监管的数据库表个数：总数与新增
        getRegulatedDatabaseTablesSize(jsonObject);

        // 4）产生告警次数：告警分布与告警处置的记录
        getAlarmRelatedData(jsonObject);

        // 5）受监管的应用清单：总量与新增
        getRegulatedApplicationList(jsonObject);

        // 6）受监管的用户数量（获取用户人数）：总量与新增
        getRegulatedUserSize(jsonObject);

        returnAllJsonObject.set(Const.REPORT_AGENT_SERVER_OPERATION_RECORD_NAME, jsonObject);
    }

    /**
     * <B>方法名称：getAlarmRelatedInfo</B>
     * <B>概要说明：</B>
     *
     * @return void
     * @Author lyx
     * @Date 2022-12-06 13:41:37
     * @Param [jsonObject]
     **/
    private void getAlarmRelatedData(ObjectNode jsonObject) {
        try {
            List<AlarmData> alarmDistributionData = segmentDetailService.getAlarmDistributionData();
            List<MsAlarmInformationDo> alarmHandledData = segmentDetailService.getAlarmHandledData();
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_ALARM_RELATED_DATA_DESC);
            reportAgentServerNameJson.put(Const.REPORT_ALARM_DISTRIBUTION_DATA, JsonUtil.obj2String(alarmDistributionData));
            reportAgentServerNameJson.put(Const.REPORT_ALARM_HANDLED_DATA, JsonUtil.obj2String(alarmHandledData));
            jsonObject.set(Const.REPORT_ALARM_RELATED_DATA_NAME, reportAgentServerNameJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getAlarmRelatedData() # 获取告警分布与告警处置的记录时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getRegulatedUserSize</B>
     * <B>概要说明：受监管的用户数量（获取用户人数）：总量与新增</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 09:41:37
     * @Param [jsonObject]
     **/
    private void getRegulatedUserSize(ObjectNode jsonObject) {
        try {
            Long userCountFromRedis = mingshiServerUtil.getUserCount();
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_REGULATED_NUMBER_OF_USER_DESC);
            reportAgentServerNameJson.put(Const.REPORT_REGULATED_NUMBER_OF_USER, userCountFromRedis);
            // todo：需要与上次出报告数据进行对比，找到新增的受监管的应用清单；
            jsonObject.set(Const.REPORT_REGULATED_NUMBER_OF_USER_NAME, reportAgentServerNameJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getRegulatedUserSize() # 获取受监管的用户数量（获取用户人数）：总量与新增时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getRegulatedApplicationList</B>
     * <B>概要说明：受监管的应用清单：总量与新增</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 09:30:16
     * @Param [jsonObject]
     **/
    private void getRegulatedApplicationList(ObjectNode jsonObject) {
        try {
            List<MsAgentInformationDo> userPortraitRulesDoList = msAgentInformationMapper.selectAllAgents(new HashMap<>(Const.NUMBER_EIGHT));
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_REGULATED_APPLICATION_LIST_DESC);
            reportAgentServerNameJson.put(Const.REPORT_REGULATED_APPLICATION_LIST, JsonUtil.obj2String(userPortraitRulesDoList));
            reportAgentServerNameJson.put(Const.REPORT_REGULATED_APPLICATION_SIZE, userPortraitRulesDoList.size());
            // todo：需要与上次出报告数据进行对比，找到新增的受监管的应用清单；
            jsonObject.set(Const.REPORT_REGULATED_APPLICATION_LIST_NAME, reportAgentServerNameJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getRegulatedApplicationList() # 获取受监管的应用清单：总量与新增时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getRegulatedDatabaseTablesSize</B>
     * <B>概要说明：受监管的数据库表个数：总数与新增</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 09:27:16
     * @Param [jsonObject]
     **/
    private void getRegulatedDatabaseTablesSize(ObjectNode jsonObject) {
        try {
            Integer dbInstanceCount = mingshiServerUtil.getDbCount();
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_REGULATED_DATABASE_TABLES_SIZE_DESC);
            reportAgentServerNameJson.put(Const.REPORT_REGULATED_DATABASE_TABLES_SIZE, dbInstanceCount);
            // todo：需要与上次出报告数据进行对比，找到新增的数据库表；

            jsonObject.set(Const.REPORT_REGULATED_DATABASE_TABLES_SIZE_NAME, reportAgentServerNameJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getRegulatedDatabaseTablesSize() # 获取受监管的数据库表个数：总数与新增时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getDataAccessBehavior</B>
     * <B>概要说明：数据访问行为数量</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 09:22:42
     * @Param [jsonObject]
     **/
    private void getDataAccessBehavior(ObjectNode jsonObject) {
        try {
            Long informationCount = mingshiServerUtil.getRecordCount();
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_DATA_ACCESS_BEHAVIOR_DESC);
            reportAgentServerNameJson.put(Const.REPORT_DATA_ACCESS_BEHAVIOR, informationCount);
            jsonObject.set(Const.REPORT_DATA_ACCESS_BEHAVIOR_NAME, reportAgentServerNameJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getDataAccessBehavior() # 获取数据访问行为数量时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getReportAgentServerName</B>
     * <B>概要说明：获取可视化系统运行时长</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-05 15:12:48
     * @Param [jsonObject]
     **/
    private void getReportAgentServerName(ObjectNode jsonObject) {
        try {
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            List<MsSystemOperationRecord> msSystemOperationRecordList = msSystemOperationRecordMapper.selectBySystemName(Const.REPORT_AGENT_SERVER_NAME);
            for (int i = 0; i < msSystemOperationRecordList.size(); i++) {
                MsSystemOperationRecord msSystemOperationRecord = msSystemOperationRecordList.get(i);
                if (null != msSystemOperationRecord && null != msSystemOperationRecord.getGmtCreate() && null != msSystemOperationRecord.getGmtModified()) {
                    Date gmtCreate = msSystemOperationRecord.getGmtCreate();
                    Date gmtModified = msSystemOperationRecord.getGmtModified();
                    Instant gmtCreateInstant = gmtCreate.toInstant();
                    Instant gmtModifiedInstant = gmtModified.toInstant();

                    long toHours = Duration.between(gmtCreateInstant, gmtModifiedInstant).toHours();
                    reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_AGENT_SERVER_NAME_DESC);
                    reportAgentServerNameJson.put(Const.OPERATION_TIME, toHours);
                    jsonObject.set(Const.REPORT_AGENT_SERVER_NAME, reportAgentServerNameJson);
                }
            }
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getReportAgentServerName() # 获取可视化系统运行时长时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getSingleRegulatedApplicationRunTime</B>
     * <B>概要说明：获取单个业务系统的运行时长</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-12-06 15:13:11
     * @Param [jsonObject]
     **/
    private void getSingleRegulatedApplicationRunTime(ObjectNode jsonObject) {
        try {
            ObjectNode reportAgentServerNameJson = JsonUtil.createJsonObject();
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_SINGLE_APPLICATION_RUN_TIME_LIST_DESC);
            ArrayNode jsonArray = JsonUtil.createJsonArray();
            List<MsSystemOperationRecord> msSystemOperationRecordList = msSystemOperationRecordMapper.selectBySystemName(Const.REPORT_SINGLE_REGULATED_APPLICATION_NAME);
            for (MsSystemOperationRecord msSystemOperationRecord : msSystemOperationRecordList) {
                if (null != msSystemOperationRecord && null != msSystemOperationRecord.getGmtCreate() && null != msSystemOperationRecord.getGmtModified()) {
                    Date gmtCreate = msSystemOperationRecord.getGmtCreate();
                    Date gmtModified = msSystemOperationRecord.getGmtModified();
                    Instant gmtCreateInstant = gmtCreate.toInstant();
                    Instant gmtModifiedInstant = gmtModified.toInstant();
                    long toHours = Duration.between(gmtCreateInstant, gmtModifiedInstant).toHours();
                    ObjectNode reportJson = JsonUtil.createJsonObject();
                    reportJson.put(Const.OPERATION_TIME, toHours + Const.REPORT_HOURS);
                    reportJson.put(Const.SERVICE_CODE, msSystemOperationRecord.getServiceCode());
                    jsonArray.add(reportJson);
                }
            }
            reportAgentServerNameJson.set(Const.REPORT_SINGLE_APPLICATION_RUN_TIME_LIST, jsonArray);
            jsonObject.set(Const.REPORT_SINGLE_REGULATED_APPLICATION_NAME, reportAgentServerNameJson);
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getSingleRegulatedApplicationRunTime() # 获取单个业务系统的运行时长时，出现了异常。", e);
        }
    }

    @Override
    public ServerResponse<String> getLatestReport() {

        return null;
    }
}
