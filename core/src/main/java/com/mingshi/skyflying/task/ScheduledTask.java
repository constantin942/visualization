package com.mingshi.skyflying.task;

import com.mingshi.skyflying.common.constant.Const;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * <B>主类名称: ScheduledTask</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * Date 2022/5/26 17:25
 * @Version 1.0
 **/
@Component
@Slf4j
public class ScheduledTask {
  @Resource
  private ExecitonScheduledTaskList execitonScheduledTaskList;

  /**
   * <B>方法名称：scheduledGetSegmentDetailDo</B>
   * <B>概要说明：定时从 ms_segment_detail_username_is_null 表中获取用户名不为空的记录</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 15:08:37
   * @Param []
   **/
  /**
   * 每隔30分钟执行一次：
   */
  @Scheduled(cron = "0 */1 * * * ?")
  /**
   * 每隔2小时执行一次；
   */
  // @Scheduled(cron = "0 0 0/2 * * ?")
  public void scheduledGetSegmentDetailDo() {
    execitonScheduledTaskList.doScheduledGetSegmentDetailDo(Const.SCHEDULED_GET_SEGMENT_DETAIL_DO);
  }

  /**
   * <B>方法名称：scheduledUpdateUserNameByToken</B>
   * <B>概要说明：定时基于token更新用户名</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 15:08:36
   * @Param []
   **/
  // 每隔30分钟执行一次：
  @Scheduled(cron = "0 */3 * * * ?")
  // 每隔2小时执行一次；
  // @Scheduled(cron = "0 0 0/2 * * ?")
  public void scheduledUpdateUserNameByToken() {
    execitonScheduledTaskList.doScheduledUpdateUserNameByToken(Const.SCHEDULED_UPDATE_USER_NAME_BY_TOKEN);
  }

  /**
   * <B>方法名称：scheduledUpdateUserNameByGlobalTraceId</B>
   * <B>概要说明：定时基于globalTraceId更新用户名</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 15:08:36
   * @Param []
   **/
  // 每隔30分钟执行一次：
  @Scheduled(cron = "0 */3 * * * ?")
  // 每隔2小时执行一次；
  // @Scheduled(cron = "0 0 0/2 * * ?")
  public void scheduledUpdateUserNameByGlobalTraceId() {
    execitonScheduledTaskList.doScheduledUpdateUserNameByGlobalTraceId(Const.SCHEDULED_UPDATE_USER_NAME_BY_GLOBAL_TRACEID);
  }

  /**
   * <B>方法名称：scheduledGetDmsAuditLog</B>
   * <B>概要说明：定时获取dms的审计日志</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年05月26日 17:05:44
   * @Param []
   **/
  /**
   * 每隔30分钟执行一次：
   */
  @Scheduled(cron = "0 */1 * * * ?")
  // @Scheduled(cron = "0 */30 * * * ?")
  public void scheduledGetDmsAuditLog() {
    execitonScheduledTaskList.doScheduledGetDmsAuditLog(Const.SCHEDULED_GET_DMS_AUDIT_LOG);
  }
}
