package com.mingshi.skyflying.task;

import com.mingshi.skyflying.constant.Const;
import com.mingshi.skyflying.dao.MsScheduledTaskDao;
import com.mingshi.skyflying.domain.MsScheduledTaskDo;
import com.mingshi.skyflying.service.AuditLogService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.Date;

/**
 * <B>主类名称: ScheduledTask</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/5/26 17:25
 *
 * @Version 1.0
 **/
@Component
@Slf4j
public class ScheduledTask {
  @Resource
  private AuditLogService auditLogService;
  @Resource
  private MsScheduledTaskDao msScheduledTaskDao;

  /**
   * <B>方法名称：scheduledGetDmsAuditLog</B>
   * <B>概要说明：定时获取dms的审计日志</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月26日 17:05:44
   * @Param []
   **/
  // 每隔30分钟执行一次：
  @Scheduled(cron = "0 */1 * * * ?")
  public void scheduledGetDmsAuditLog() {
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。");
    Instant start = Instant.now();
    String startTime = null;
    String endTime = null;
    MsScheduledTaskDo msScheduledTaskDo = msScheduledTaskDao.selectLastSuccessRecord(Const.RETCH_AUDIT_LOG_BY_DMS_SUCCESS_RESULT);
    if (null == msScheduledTaskDo || StringUtil.isBlank(msScheduledTaskDo.getStartTime())) {
      // 如果定时任务表里不存在操作记录，那么就设置一个默认值；2022-05-26 17:37:27
      // startTime = "2022-05-26 00:00:00";
      startTime = "1990-01-01 00:00:00";
    } else {
      // 获取上一次成功执行完毕的操作事件；2022-05-26 17:37:56
      startTime = msScheduledTaskDo.getEndTime();
    }

    endTime = DateTimeUtil.dateToStrformat(new Date());
    log.info("开始执行 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。开始时间 startTime = 【{}】，endTime = 【{}】。", startTime, endTime);
    auditLogService.autoFetchAuditlogByDMS(startTime, endTime);
    log.info("执行结束 #scheduledGetDmsAuditLog.scheduledGetDmsAuditLog()# 定时获取dms的审计日志。耗时 = 【{}】毫秒。", DateTimeUtil.getTimeMillis(start));
  }
}
