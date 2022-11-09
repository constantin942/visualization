package com.mingshi.skyflying.anomaly_detection.task;

import com.mingshi.skyflying.anomaly_detection.caffeine.MsCaffeineCache;
import com.mingshi.skyflying.anomaly_detection.dao.DingAlarmConfigMapper;
import com.mingshi.skyflying.anomaly_detection.dao.DingAlarmInformationMapper;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmConfig;
import com.mingshi.skyflying.anomaly_detection.domain.DingAlarmInformation;
import com.mingshi.skyflying.common.constant.AnomalyConst;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.enums.AlarmEnum;
import com.mingshi.skyflying.common.utils.DingUtils;
import com.mingshi.skyflying.common.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <B>方法名称：DingAlarmTask</B>
 * <B>概要说明：定时任务类</B>
 *
 * @Author: 李宇翔
 * @Description: 定时发送钉钉消息类
 * @Date: create in 2022/11/9
 */
@Slf4j
@Configuration
@EnableScheduling
public class DingAlarmTask {

    @Resource
    DingAlarmInformationMapper dingAlarmInformationMapper;

    @Resource
    DingAlarmConfigMapper dingAlarmConfigMapper;

    /**
     * <B>方法名称：sendDingAlarm</B>
     * <B>概要说明：间歇发送钉钉告警信息</B>
     *
     * @Author lyx
     * @Date 2022-11-09 10:35:44
     * @Param []
     **/
    @Scheduled(cron = "0 */1 * * * ?")
//    @Scheduled(cron = "0 */5 * * * ?")
    @Async
    public void sendDingAlarm() throws InterruptedException {
        int index = 0;
        while (!MsCaffeineCache.getDingInfoInsertedDone() && index < Const.NUM_FIVE) {
            Thread.sleep(6000);
            index++;
        }
        DingAlarmConfig dingAlarmConfig = dingAlarmConfigMapper.selectOne();
        Integer gap = dingAlarmConfig.getGap();
        List<DingAlarmInformation> dingAlarmInformationList = dingAlarmInformationMapper.selectPeriodInfo(AnomalyConst.SECONDS * gap);
        if (dingAlarmInformationList.isEmpty()) {return;}
        try {
            String message = "";
            for (DingAlarmInformation dingAlarmInformation : dingAlarmInformationList) {
                message = buildDingAlarmInfo(message, dingAlarmInformation);
            }
            List<String> mobiles = null;
            if (!StringUtil.isEmpty(dingAlarmConfig.getMobiles())) {
                mobiles = Arrays.stream(dingAlarmConfig.getMobiles().split(Const.POUND_KEY)).collect(Collectors.toList());
            }
            DingUtils.dingRequest(message, dingAlarmConfig.getWebhook(), dingAlarmConfig.getSecret(), mobiles);
            log.info("钉钉告警成功");
            for (DingAlarmInformation dingAlarmInformation : dingAlarmInformationList) {
                dingAlarmInformation.setIsDelete(Const.IS_DELETE_ONE);
                dingAlarmInformationMapper.updateByPrimaryKeySelective(dingAlarmInformation);
            }
            log.info("钉钉告警逻辑信息删除成功");
        } catch (Exception e) {
            log.error("钉钉告警发生异常:{}", e.getMessage());
        }
    }

    /**
     * 构建钉钉告警内容
     */
    private String buildDingAlarmInfo(String message, DingAlarmInformation dingAlarmInformation) {
        StringBuilder sb = new StringBuilder(message);
        if (!StringUtil.isEmpty(sb.toString())) {
            sb.append("\n\r");
        }
        sb.append(dingAlarmInformation.getCreateTime());
        sb.append(" :\n\r\t用户").append(dingAlarmInformation.getUsername());
        Integer code = dingAlarmInformation.getRuleId();
        if (code.equals(AlarmEnum.TIME_ALARM.getCode())) {
            sb.append("以往在该时段不经常访问,一共访问了");
            sb.append(dingAlarmInformation.getTriggerTimes());
            sb.append("次");
            return sb.toString();
        }
        if (code.equals(AlarmEnum.TABLE_ALARM.getCode())) {
            sb.append("访问了不经常使用的表");
            sb.append(dingAlarmInformation.getTriggerTimes());
            sb.append("次");
            return sb.toString();
        }
        sb.append("进行了高危操作");
        sb.append(dingAlarmInformation.getTriggerTimes());
        sb.append("次");
        return sb.toString();
    }
}
