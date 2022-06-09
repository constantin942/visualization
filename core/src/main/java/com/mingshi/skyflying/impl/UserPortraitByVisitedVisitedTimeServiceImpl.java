package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTimeMapper;
import com.mingshi.skyflying.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.enums.ConstantsCode;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.UserPortraitByVisitedTimeService;
import com.mingshi.skyflying.utils.DateTimeUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;

// TODO：与弘毅讨论下这个异常检测该怎么实现？
// 2022-06-06 17:02:50
// 异常检测规则有：
// 1. 若某用户通常白天访问数据，则夜间为异常；
// 2. 某用户访问从未访问过的表；
// 3. 访问频率、访问量激增；
/**
 * <B>方法名称：UserPortraitByTimeServiceImpl</B>
 * <B>概要说明：根据用户访问系统的时间，来计算其时间维度的画像。</B>
 *    工作流程：
 *       a. 首先根据历史数据，初步生成用户的访问时间维度的画像，并存储到数据库中；
 *       b. 项目启动的时候，从数据库中加载访问时间维度的画像到jvm本地内存中；
 *       c. 消费者拿到用户访问系统的链路信息之后，
 *          c1. 如果该链路中没有用户名，则跳过异常检测。由后续的定时任务来执行异常检测（后续更新了用户名信息），其具体实施步骤如下：
 *              c11. 从ms_segment_detail表中，将用户名不为空且未检测过的基于访问时间的记录查询出来，然后走异常判断逻辑。
 *          c2. 如果该链路信息中有用户名，那么就进行访问时间维度的异常检测。同时将该记录标识为已进行了过了基于访问时间的异常检测；
 *          c3. 如果发生异常，则将异常信息保存到数据库中；
 *          c4. 如果没有发生异常，则将用户在当前时间段内的访问次数累加1。然后定时将累加后的数据更新到MySQL数据库中；
 * 目前这个功能已实现，能够满足：a. 实时的检测异常行为；b. 分钟级别更新用户画像信息；
 * @Author zm
 * @Date 2022年06月07日 14:06:43
 * @Param
 * @return
 **/
@Slf4j
@Service("userPortraitByVisitedTimeService")
public class UserPortraitByVisitedVisitedTimeServiceImpl implements UserPortraitByVisitedTimeService {

  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private UserPortraitByVisitedTimeMapper userPortraitByVisitedTimeMapper;

  @Override
  public ServerResponse<String> createUserPortraitByVisitedTime() {
    Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> statisticsMap = new HashMap<>();

    // 基于时间，统计用户的访问次数
    statisticsVistedCount(statisticsMap);

    // 批量插入用户的访问统计数据
    batchInsertUserPortrait(statisticsMap);

    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：statisticsVistedCount</B>
   * <B>概要说明：基于时间，统计用户的访问次数</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月07日 16:06:44
   * @Param [statisticsMap]
   **/
  private void statisticsVistedCount(Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> statisticsMap) {
    Instant selectNow = Instant.now();
    // 获取所有的历史数据，根据用户访问系统的时间，来计算其时间维度的画像。
    List<MsSegmentDetailDo> list = msSegmentDetailDao.selectAllUserNameIsNotNull();
    log.info("# UserPortraitByTimeServiceImpl.statisticsVistedCount() # 从数据库中查询出【{}】条用户的访问后端服务的追踪信息用时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(selectNow));
    if (null != list && 0 < list.size()) {
      for (MsSegmentDetailDo msSegmentDetailDo : list) {
        String userName = msSegmentDetailDo.getUserName();
        String startTimeStr = msSegmentDetailDo.getStartTime();
        msSegmentDetailDo.setUserPortraitFlagByVisitedTime(1);
        Date date = DateTimeUtil.strToDate(startTimeStr);
        String currHourTime = DateTimeUtil.judgmentTime(date);

        Map<String, Integer> timeCountMap = statisticsMap.get(userName);
        if (null == timeCountMap) {
          timeCountMap = new HashMap<>();
          statisticsMap.put(userName, timeCountMap);
          timeCountMap.put(currHourTime, 1);
        } else {
          Integer count = timeCountMap.get(currHourTime);
          if (null == count) {
            timeCountMap.put(currHourTime, 1);
          } else {
            timeCountMap.put(currHourTime, count + 1);
          }
        }
      }
      try {
        // TODO：不应该放在这里更新，正确的做法是：当统计信息正常插入到数据库中之后，才能更新。否则，会造成这里已经更新成功，但统计信息插入到数据库失败的情况。2022-06-08 16:59:50
        Instant updateNow = Instant.now();
        msSegmentDetailDao.updateBatchById(list);
        log.info("# UserPortraitByTimeServiceImpl.statisticsVistedCount # 批量更新基于访问时间的【{}条】标识耗时 = 【{}】毫秒。",list.size(),DateTimeUtil.getTimeMillis(updateNow));
      } catch (Exception e) {
        log.error("# UserPortraitByTimeServiceImpl.statisticsVistedCount # 批量更新基于访问时间的信息标识出现了异常。", e);
      }
    }
  }

  /**
   * <B>方法名称：batchInsertUserPortrait</B>
   * <B>概要说明：批量插入用户的访问统计数据</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月07日 16:06:35
   * @Param [list]
   **/
  private void batchInsertUserPortrait(Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> statisticsMap) {
    try {
      List<UserPortraitByVisitedTimeDo> list = new LinkedList<>();
      if (null == statisticsMap || 0 == statisticsMap.size()) {
        return;
      }
      Iterator<String> iterator = statisticsMap.keySet().iterator();
      while (iterator.hasNext()) {
        UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo = new UserPortraitByVisitedTimeDo();
        String userName = iterator.next();
        userPortraitByVisitedTimeDo.setUserName(userName);
        Map<String, Integer> map = statisticsMap.get(userName);
        Iterator<String> iterator2 = map.keySet().iterator();
        while (iterator2.hasNext()) {
          String time = iterator2.next();
          Integer count = map.get(time);
          if (time.equals(ConstantsCode.USER_PORTRAIT_NIGHT.getMsgEn())) {
            userPortraitByVisitedTimeDo.setNightCount(count);
          } else if (time.equals(ConstantsCode.USER_PORTRAIT_FORENOON.getMsgEn())) {
            userPortraitByVisitedTimeDo.setForenoonCount(count);
          } else if (time.equals(ConstantsCode.USER_PORTRAIT_AFTERNOON.getMsgEn())) {
            userPortraitByVisitedTimeDo.setAfternoonCount(count);
          } else {
            log.error("# UserPortraitByTimeServiceImpl.createUserPortraitByTime() # 基于时间统计用户的访问次数时，出现了错误。当前时间 = 【{}】既不是上午、下午，也不是晚上。这是不对的，需要排查错误原因。", time);
          }
        }
        list.add(userPortraitByVisitedTimeDo);
      }
      if (0 < list.size()) {
        Instant now = Instant.now();
        userPortraitByVisitedTimeMapper.insertSelectiveBatch(list);
        log.info("# UserPortraitByTimeServiceImpl.batchInsertUserPortrait() # 将统计好的用户访问次数【{}条】批量插入到数据库中用时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# UserPortraitByTimeServiceImpl.batchInsertUserPortrait() # 将统计好的用户访问次数批量插入到数据库中出现了异常。", e);
    }
  }

}
