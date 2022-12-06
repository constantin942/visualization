package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.dao.MsAgentInformationMapper;
import com.mingshi.skyflying.common.dao.MsSystemOperationRecordMapper;
import com.mingshi.skyflying.common.dao.MsUserFromMapper;
import com.mingshi.skyflying.common.domain.MsAgentInformationDo;
import com.mingshi.skyflying.common.domain.MsReport;
import com.mingshi.skyflying.common.domain.MsSystemOperationRecord;
import com.mingshi.skyflying.common.domain.MsUserFrom;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.MingshiServerUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.service.ReportService;
import com.mingshi.skyflying.service.UserFromService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import scala.App;

import javax.annotation.Resource;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    private MsAgentInformationMapper msAgentInformationMapper;
    @Resource
    private MsSystemOperationRecordMapper msSystemOperationRecordMapper;

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
     * @Author zm
     * @Date 2022-12-06 10:01:13
     * @Param [returnAllJsonObject]
     * @return void
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
     * @Author zm
     * @Date 2022-12-06 10:00:24
     * @Param []
     * @return void
     **/
    private void getSingleApplicationRecord(ObjectNode returnAllJsonObject) {
        /**
         * 2. 单个业务应用系统
         * 1）运行时长；
         * 2）用户数量：总量与新增；
         * 3）访问行为数；
         * 4）相关数据库表清单和次数；
         * 5）调用数据接口的清单和次数；
         * 6）告警次数：告警分布和遗留问题记录；
         * 7）同比上期的结果；
         * 8）历史各次总结的走势；
         */
        //运行时长

        // 用户数量：总量与新增

        // 访问行为数

        // 相关数据库表清单和次数

        // 调用数据接口的清单和次数

        // 告警次数：告警分布和遗留问题记录

        // 同比上期的结果

        // 历史各次总结的走势
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

        // 产生告警次数：告警分布与告警处置的记录

        // 5）受监管的应用清单：总量与新增
        getRegulatedApplicationList(jsonObject);

        // 6）受监管的用户数量（获取用户人数）：总量与新增
        getRegulatedUserSize(jsonObject);

        returnAllJsonObject.set(Const.REPORT_AGENT_SERVER_OPERATION_RECORD_NAME, jsonObject);
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
            reportAgentServerNameJson.put(Const.REPORT_DESC, Const.REPORT_REGULATED_USER_SIZE_DESC);
            reportAgentServerNameJson.put(Const.REPORT_REGULATED_USER_SIZE, userCountFromRedis);
            // todo：需要与上次出报告数据进行对比，找到新增的受监管的应用清单；
            jsonObject.set(Const.REPORT_REGULATED_USER_SIZE_NAME, reportAgentServerNameJson);
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
            MsSystemOperationRecord msSystemOperationRecord = msSystemOperationRecordMapper.selectBySystemName(Const.REPORT_AGENT_SERVER_NAME);
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
        } catch (Exception e) {
            log.error("# ReportServiceImpl.getReportAgentServerName() # 获取可视化系统运行时长时，出现了异常。", e);
        }
    }

    @Override
    public ServerResponse<String> getLatestReport() {

        return null;
    }
}
