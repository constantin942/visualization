package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.anomaly_detection.singleton.AnomylyDetectionSingletonByVisitedTime;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import com.mingshi.skyflying.common.domain.UserPortraitByVisitedTimeDo;
import com.mingshi.skyflying.common.enums.ConstantsCode;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateTimeUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.UserPortraitByVisitedTimeMapper;
import com.mingshi.skyflying.service.UserPortraitByVisitedTimeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

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
    Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> statisticsMap = new HashMap<>(Const.NUMBER_EIGHT);

    // 基于时间，统计用户的访问次数
    statisticsVistedCount(statisticsMap);

    // 批量插入用户的访问统计数据
    batchInsertUserPortrait(statisticsMap);

    return ServerResponse.createBySuccess();
  }

  @Override
  public ServerResponse<String> getAllUserPortraitByVisitedTime(String userName, Integer pageNo, Integer pageSize) {
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
    List<UserPortraitByVisitedTimeDo> userPortraitByVisitedTimeDos = userPortraitByVisitedTimeMapper.selectByUserName(queryMap);

    Integer count = userPortraitByVisitedTimeMapper.selectByUserNameCount(queryMap);

    Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
    context.put("rows", JsonUtil.obj2String(userPortraitByVisitedTimeDos));
    context.put("total", count);

    log.info("# UserPortraitByVisitedVisitedTimeServiceImpl.getAllUserPortraitByVisitedTime() # 根据条件【{}】在数据库中查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), userPortraitByVisitedTimeDos.size());
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(context));
    return bySuccess;
  }

  /**
   * <B>方法名称：updateUserPortraitByVisitedTimeRule</B>
   * <B>概要说明：禁启用用户在什么时间访问了多少次系统规则</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月16日 17:06:55
   * @Param []
   **/
  @Override
  public ServerResponse<String> updateUserPortraitByVisitedTimeRule(Integer ruleId, Integer isDelete) {
    log.info("开始执行 # UserPortraitByVisitedVisitedTimeServiceImpl.updateUserPortraitByVisitedTimeRule() # 更新用户在什么时间访问过的系统次数画像规则启用状态。 ");
    if (!isDelete.equals(Const.IS_DELETE_ZERO) && !isDelete.equals(Const.IS_DELETE_ONE)) {
      return ServerResponse.createByErrorMessage("参数非法：是否启用的参数isDelete应该是0或者1.", "");
    }

    // 先根据规则id在数据库中找到这条规则；2022-06-16 14:44:21
    UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo = userPortraitByVisitedTimeMapper.selectByPrimaryKey(ruleId);
    if (null == userPortraitByVisitedTimeDo) {
      return ServerResponse.createByErrorMessage("参数非法：规则id在数据库中不存在.", "");
    }

    Integer isDelete1 = userPortraitByVisitedTimeDo.getIsDelete();
    // 如果数据库中该条规则的状态已经是当前要更新的状态，那么就结束当前请求；2022-06-16 17:36:58
    if(isDelete1.equals(isDelete)){
      return ServerResponse.createBySuccess();
    }
    // 设置规则启用/禁用的状态；2022-06-16 15:20:09
    userPortraitByVisitedTimeDo.setIsDelete(isDelete);

    // 更新本地内存；2022-06-16 14:49:29
    ServerResponse<String> response = doUpdateUserPortraitByVisitedTimeRule(userPortraitByVisitedTimeDo, isDelete);
    log.info("执行完毕 # UserPortraitByVisitedVisitedTimeServiceImpl.updateUserPortraitByVisitedTimeRule() # 更新用户访问过的表的画像规则启用状态。 ");
    return response;
  }

  @Override
  public ServerResponse<String> addUserPortraitByVisitedTtimeRule(String userName, Integer forenoonCount, Integer afternoonCount, Integer nightCount) {
    if (StringUtil.isBlank(userName)) {
      return ServerResponse.createByErrorMessage("参数 userName 不能为空。", "");
    }
    if (null == forenoonCount || 0 > forenoonCount) {
      return ServerResponse.createByErrorMessage("参数 forenoonCount 不能为空或不能为负数。", "");
    }
    if (null == afternoonCount || 0 > afternoonCount) {
      return ServerResponse.createByErrorMessage("参数 afternoonCount 不能为空或不能为负数。", "");
    }
    if (null == nightCount || 0 > nightCount) {
      return ServerResponse.createByErrorMessage("参数 nightCount 不能为空或不能为负数。", "");
    }
    UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo = new UserPortraitByVisitedTimeDo();
    userPortraitByVisitedTimeDo.setUserName(userName);
    userPortraitByVisitedTimeDo.setForenoonCount(forenoonCount == 0 ? null : forenoonCount);
    userPortraitByVisitedTimeDo.setAfternoonCount(afternoonCount == 0 ? null : afternoonCount);
    userPortraitByVisitedTimeDo.setNightCount(nightCount == 0 ? null : nightCount);
    int insertResult = userPortraitByVisitedTimeMapper.insertSelective(userPortraitByVisitedTimeDo);
    if (1 != insertResult) {
      log.error(" # UserPortraitByVisitedVisitedTimeServiceImpl.addUserPortraitByVisitedTtimeRule() # 增加用户在什么时间访问过多少次系统规则时，插入到表中失败。");
      return ServerResponse.createByErrorMessage("数据库插入操作失败。", "");
    }

    // 将规则加入到本地内存中，这里的做法只适用于单实例部署，如果有定时任务定时将数据库中的规则加载到本地内存，也适用。2022-06-16 16:51:04
    addUserPortraitByVisitedTimeRuleToLocalMemory(userPortraitByVisitedTimeDo);

    return ServerResponse.createBySuccess();
  }


  private ServerResponse<String> doUpdateUserPortraitByVisitedTimeRule(UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo, Integer isDelete) {
    if (isDelete.equals(Const.IS_DELETE_ONE)) {
      // 禁用这条规则；2022-06-16 14:55:51
      return noEnableByUserPortraitByVisitedTime(userPortraitByVisitedTimeDo);
    }
    if (isDelete.equals(Const.IS_DELETE_ZERO)) {
      // 启用这条规则；2022-06-16 14:55:51
      return enableByUserPortraitByVisitedTime(userPortraitByVisitedTimeDo);
    }

    return null;
  }

  /**
   * <B>方法名称：noEnableByUserPortraitByVisitedTime</B>
   * <B>概要说明：将用户在什么时间访问多少次系统这个规则先在数据库中禁用，然后在本地内存中删除</B>
   * @Author zm
   * @Date 2022年06月16日 17:06:20
   * @Param [userPortraitByVisitedTimeDo]
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  private ServerResponse<String> noEnableByUserPortraitByVisitedTime(UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo) {
    int updateResult = userPortraitByVisitedTimeMapper.updateByPrimaryKeySelective(userPortraitByVisitedTimeDo);
    if (1 != updateResult) {
      log.error(" # UserPortraitByVisitedVisitedTimeServiceImpl.noEnableByUserPortraitByVisitedTime() # 把禁用这条规则的状态更新到数据库中失败。");
      return ServerResponse.createByErrorMessage("更新数据库操作失败", "");
    }

    Boolean userPortraitByVisitedTimeEnable = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeEnable();
    if(false == userPortraitByVisitedTimeEnable){
      // 这条规则没有启用，那么就直接返回；2022-06-23 16:09:28
      return ServerResponse.createBySuccess();
    }
    // 数据库操作成功，才将本地内存中的数据删除；2022-06-16 17:29:01
    Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> userVisitedDateCountMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
    String userName = userPortraitByVisitedTimeDo.getUserName();
    Map<String, Integer> visitedDateCountMap = userVisitedDateCountMap.get(userName);
    if (null != visitedDateCountMap) {
      userVisitedDateCountMap.remove(userName);
    }
    return ServerResponse.createBySuccess();
  }

  /**
   * <B>方法名称：enableByUserPortraitByVisitedTime</B>
   * <B>概要说明：将用户在什么时间访问多少次系统这个规则先在数据库中启用，然后添加到本地内存中</B>
   * @Author zm
   * @Date 2022年06月16日 17:06:20
   * @Param [userPortraitByVisitedTimeDo]
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   **/
  private ServerResponse<String> enableByUserPortraitByVisitedTime(UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo) {

    addUserPortraitByVisitedTimeRuleToLocalMemory(userPortraitByVisitedTimeDo);

    int updateResult = userPortraitByVisitedTimeMapper.updateByPrimaryKeySelective(userPortraitByVisitedTimeDo);
    if (1 != updateResult) {
      log.error(" # UserPortraitByVisitedVisitedTimeServiceImpl.enableByUserPortraitByVisitedTime() # 把启用这条规则的状态更新到数据库中失败。");
      return ServerResponse.createByErrorMessage("更新数据库操作失败", "");
    }
    return ServerResponse.createBySuccess();
  }

  private void addUserPortraitByVisitedTimeRuleToLocalMemory(UserPortraitByVisitedTimeDo userPortraitByVisitedTimeDo) {
    Boolean userPortraitByVisitedTimeEnable = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeEnable();
    if(false == userPortraitByVisitedTimeEnable){
      // 这条规则没有启用，那么就直接返回；2022-06-23 16:09:28
      return;
    }
    Map<String/* 用户名 */, Map<String/* 访问时间 */, Integer/* 在当前时间段内的访问次数 */>> userVisitedDateCountMap = AnomylyDetectionSingletonByVisitedTime.getUserPortraitByVisitedTimeMap();
    String userName = userPortraitByVisitedTimeDo.getUserName();
    Integer forenoonCount = userPortraitByVisitedTimeDo.getForenoonCount();
    Integer nightCount = userPortraitByVisitedTimeDo.getNightCount();
    Integer afternoonCount = userPortraitByVisitedTimeDo.getAfternoonCount();
    Map<String, Integer> visitedDateCountMap = userVisitedDateCountMap.get(userName);
    if (null == visitedDateCountMap) {
      visitedDateCountMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
      userVisitedDateCountMap.put(userName, visitedDateCountMap);
    }
    if(null != forenoonCount && 0 <= forenoonCount){
      visitedDateCountMap.put(ConstantsCode.USER_PORTRAIT_FORENOON.getCode(),forenoonCount);
    }
    if(null != afternoonCount && 0 <= afternoonCount){
      visitedDateCountMap.put(ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode(),afternoonCount);
    }
    if(null != nightCount && 0 <= nightCount){
      visitedDateCountMap.put(ConstantsCode.USER_PORTRAIT_NIGHT.getCode(),nightCount);
    }
  }


  @Override
  public ServerResponse<String> getAllUserNamePortraitByVisitedTime() {
    List<String> list = userPortraitByVisitedTimeMapper.selectAllUserName();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    bySuccess.setData(JsonUtil.obj2String(list));
    return bySuccess;
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
    if (null != list && !list.isEmpty()) {
      for (MsSegmentDetailDo msSegmentDetailDo : list) {
        String userName = msSegmentDetailDo.getUserName();
        String startTimeStr = msSegmentDetailDo.getStartTime();
        msSegmentDetailDo.setUserPortraitFlagByVisitedTime(1);
        Date date = DateTimeUtil.strToDate(startTimeStr);
        String currHourTime = DateTimeUtil.judgmentTime(date);

        Map<String, Integer> timeCountMap = statisticsMap.get(userName);
        if (null == timeCountMap) {
          timeCountMap = new HashMap<>(Const.NUMBER_EIGHT);
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
          if (time.equals(ConstantsCode.USER_PORTRAIT_NIGHT.getCode())) {
            userPortraitByVisitedTimeDo.setNightCount(count);
          } else if (time.equals(ConstantsCode.USER_PORTRAIT_FORENOON.getCode())) {
            userPortraitByVisitedTimeDo.setForenoonCount(count);
          } else if (time.equals(ConstantsCode.USER_PORTRAIT_AFTERNOON.getCode())) {
            userPortraitByVisitedTimeDo.setAfternoonCount(count);
          } else {
            log.error("# UserPortraitByTimeServiceImpl.createUserPortraitByTime() # 基于时间统计用户的访问次数时，出现了错误。当前时间 = 【{}】既不是上午、下午，也不是晚上。这是不对的，需要排查错误原因。", time);
          }
        }
        list.add(userPortraitByVisitedTimeDo);
      }
      if (!list.isEmpty()) {
        Instant now = Instant.now();
        userPortraitByVisitedTimeMapper.insertSelectiveBatch(list);
        log.info("# UserPortraitByTimeServiceImpl.batchInsertUserPortrait() # 将统计好的用户访问次数【{}条】批量插入到数据库中用时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
      }
    } catch (Exception e) {
      log.error("# UserPortraitByTimeServiceImpl.batchInsertUserPortrait() # 将统计好的用户访问次数批量插入到数据库中出现了异常。", e);
    }
  }

}
