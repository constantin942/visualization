package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.SysOperator;
import com.mingshi.skyflying.common.domain.SysOperatorRole;
import com.mingshi.skyflying.common.domain.UserLoginStatistics;
import com.mingshi.skyflying.common.exception.AiitExceptionCode;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.UserUtil;
import com.mingshi.skyflying.dao.SysOperatorDao;
import com.mingshi.skyflying.dao.SysOperatorRoleDao;
import com.mingshi.skyflying.service.AiitSysUsersService;
import com.mingshi.skyflying.service.UserLoginStatisticsService;
import jodd.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @author 49090 2017年12月26日上午11:34:48
 * 本内容仅限于北大信研院安全工程与云计算实验室内部传阅，禁止外泄以及用于其他的商业目的
 * @类描述：
 */
@Slf4j
@Service
public class SysUsersServiceImpl extends BaseParentServiceImpl<SysOperator, Long> implements AiitSysUsersService {

  @Resource
  private SysOperatorDao sysOperatorDao;
  @Resource
  private SysOperatorRoleDao sysOperatorRoleDao;
  @Resource
  private UserUtil userUtil;
  @Resource
  private UserLoginStatisticsService userLoginStatisticsService;

  @Override
  public int insertSelective(SysOperator sysOperator) {
    return sysOperatorDao.insertSelective(sysOperator);
  }

  @Override
  public SysOperator selectByPrimaryKey(Integer id) {
    return sysOperatorDao.selectByPrimaryKey(id);
  }

  @Override
  public int updateByPrimaryKeySelective(SysOperator sysOperator) {
    return sysOperatorDao.updateByPrimaryKeySelective(sysOperator);
  }

  @Override
  public ServerResponse<String> changePassword(String userName, String oldPassword, String newPassword) {
    ServerResponse<String> serverResponse = new ServerResponse<>(AiitExceptionCode.SUCCESS);
    /** 判断传递的参数是否为空*/
    if (StringUtil.isBlank(oldPassword)) {
      return new ServerResponse<>(AiitExceptionCode.OLD_PASSWORD_IS_EMPTY);
    }
    if (StringUtil.isBlank(newPassword)) {
      return new ServerResponse<>(AiitExceptionCode.NEW_PASSWORD_IS_EMPTY);
    }
    if (StringUtil.isBlank(userName)) {
      return new ServerResponse<>(AiitExceptionCode.USERNAME_IS_EMPTY);
    }
    /** 判断用户名是否存在*/
    SysOperator sysOperator = sysOperatorDao.selectByUserName(userName);
    if (null == sysOperator) {
      return new ServerResponse<>(AiitExceptionCode.USERNAME_DOES_NOT_EXIST);
    }

    /** 判断新密码与旧密码是否一样*/
    if (StringUtil.equals(oldPassword, newPassword)) {
      return new ServerResponse<>(AiitExceptionCode.PASSWORD_IS_SAME);
    }

    /** 判断新密码长度是否太长*/
    if (newPassword.length() > Const.DATA_LENTGH) {
      return new ServerResponse<>(AiitExceptionCode.NEW_PASSWORD_TOO_LONG);
    }

    /** 获取盐值*/
    String salt = sysOperator.getSalt();
    /** 将密码与盐值一起加密，防止密码明文存储*/
    String oldPasswordSalt = userUtil.getPassword(oldPassword, salt);
    if (!StringUtil.equals(oldPasswordSalt, sysOperator.getPassword())) {
      return new ServerResponse<>(AiitExceptionCode.OLD_PASSWORD_IS_ERROR);
    }

    String newPasswordSalt = userUtil.getPassword(newPassword, salt);
    sysOperator.setPassword(newPasswordSalt);

    int updatePassword = sysOperatorDao.updateByPrimaryKeySelective(sysOperator);
    if (updatePassword == 0) {
      log.error("用户修改密码，更新用户={}的密码失败。", userName);
      return new ServerResponse<>(AiitExceptionCode.FAILURE);
    }
    return serverResponse;
  }


  @Override
  public ServerResponse<String> login(String userName, String password) {
    if (StringUtil.isBlank(userName)) {
      return new ServerResponse<String>(AiitExceptionCode.USERNAME_IS_EMPTY);
    }
    if (StringUtil.isBlank(password)) {
      return new ServerResponse<String>(AiitExceptionCode.PASSWORD_IS_EMPTY);
    }
    SysOperator aiitUsers = sysOperatorDao.selectByUserName(userName);
    if (null == aiitUsers) {
      return new ServerResponse<>(AiitExceptionCode.USERNAME_DOES_NOT_EXIST);
    }


    /**判断密码是否有效，以及错误次数是否超限*/
    ServerResponse<String> serverResponse1 = passwordIsValid(aiitUsers, password);
    if (!StringUtil.equals(serverResponse1.getCode(), AiitExceptionCode.SUCCESS.getCode())) {
      return serverResponse1;
    }

    aiitUsers.setPassword(StringUtils.EMPTY);
    aiitUsers.setSalt(StringUtils.EMPTY);

    ServerResponse serverResponse = new ServerResponse<>(AiitExceptionCode.SUCCESS);
    serverResponse.setData(JsonUtil.obj2String(aiitUsers));
    return serverResponse;
  }

  /**
   * @Author zhaoming
   * @Description 判断密码是否有效，以及错误次数是否超限
   * @Date 13:48 2020/2/17
   * @Param [aiitUsers, password]
   **/
  private ServerResponse<String> passwordIsValid(SysOperator sysOperator, String password) {
    String userName = sysOperator.getUserName();
    UserLoginStatistics userLoginStatistics = userLoginStatisticsService.selectPasswordErrorCount(userName);
    /**判断密码是否正确*/
    String passwordCurrent = userUtil.getPassword(password, sysOperator.getSalt());
    if (StringUtils.equals(passwordCurrent, sysOperator.getPassword())) {

      /**用户输入的密码错误次数超过5次且距离最近一次的密码错误时间小于1小时，那么不可以在登录了*/
      ServerResponse<String> serverResponse = isExpired(userLoginStatistics);
      if (!StringUtil.equals(AiitExceptionCode.SUCCESS.getCode(), serverResponse.getCode())) {
        return serverResponse;
      }

      if (null != userLoginStatistics && 0 < userLoginStatistics.getPasswordErrorCount()) {
        userLoginStatistics.setGmtModified(DateUtil.dateStr4(new Date()));
        userLoginStatistics.setPasswordErrorCount(0);
        Integer updateResult = userLoginStatisticsService.updateByPrimaryKeySelective(userLoginStatistics);
        if (1 != updateResult) {
          log.error("用户={} 执行登录操作，密码输入错误，将用户的登录错误信息更新到表中失败。", userName);
        }
      }
      return new ServerResponse<>(AiitExceptionCode.SUCCESS);
    }

    Integer passwordErrorCount = 1;
    Map<String, Object> map = new HashMap<>(Const.INITAL_SIZE);

    /**先判断在一小时内，密码的错误次数是否达到了5次，若是，则一小时后再登录*/
    if (null == userLoginStatistics) {
      log.error("用户={} 执行登录操作，密码输入错误。", userName);
      userLoginStatistics = new UserLoginStatistics();
      userLoginStatistics.setUserName(userName);
      userLoginStatistics.setPasswordErrorCount(1);
      userLoginStatistics.setGmtModified(DateUtil.dateStr4(new Date()));
      Integer insertResult = userLoginStatisticsService.insertSelective(userLoginStatistics);
      if (1 != insertResult) {
        log.error("用户={} 执行登录操作，密码输入错误，将用户的登录错误信息插入表中失败。", userName);
      }
    } else {
      /**用户输入的密码错误次数超过5次且距离最近一次的密码错误时间小于1小时，那么不可以在登录了*/
      ServerResponse<String> serverResponse = isExpired(userLoginStatistics);
      if (!StringUtil.equals(AiitExceptionCode.SUCCESS.getCode(), serverResponse.getCode())) {
        return serverResponse;
      }

      passwordErrorCount = userLoginStatistics.getPasswordErrorCount() + 1;
      log.error("用户={} 登录错误次数是={}", userName, passwordErrorCount);
      userLoginStatistics.setPasswordErrorCount(passwordErrorCount);
      userLoginStatistics.setGmtModified(DateUtil.dateStr4(new Date()));
      Integer updateResult = userLoginStatisticsService.updateByPrimaryKeySelective(userLoginStatistics);
      if (1 != updateResult) {
        log.error("用户={} 执行登录操作，密码输入错误，将用户的登录错误信息更新到表中失败。", userName);
      }
    }

    map.put("passwordErrorCount", passwordErrorCount);
    ServerResponse<String> serverResponse = new ServerResponse<>(AiitExceptionCode.INCORRECT_PASSWORD);
    serverResponse.setData(JsonUtil.obj2String(map));
    return serverResponse;
  }

  private ServerResponse<String> isExpired(UserLoginStatistics userLoginStatistics) {
    if (null == userLoginStatistics) {
      return new ServerResponse<String>(AiitExceptionCode.SUCCESS);
    }
    Map<String, Object> map = new HashMap<>(Const.INITAL_SIZE);
    String userName = userLoginStatistics.getUserName();
    Date errorTime = DateUtil.strToDate(userLoginStatistics.getGmtModified(), DateUtil.DATE_TIME_SHORT);
    Long hours = DateUtil.getNumberOfHoursBetween(errorTime, new Date());
    if (Const.NUM_FIVE <= userLoginStatistics.getPasswordErrorCount() && hours < Const.NUM_ONE) {
      log.error("用户={} 登录错误次数={} 已经超过5次，直接返回。", userName, userLoginStatistics.getPasswordErrorCount());
      ServerResponse<String> serverResponse = new ServerResponse<>(AiitExceptionCode.PASSWORD_ERROR_MORE_THAN_FIVE_TIMES);
      map.put("passwordErrorCount", userLoginStatistics.getPasswordErrorCount());
      serverResponse.setData(JsonUtil.obj2String(map));
      return serverResponse;
    }
    return new ServerResponse<>(AiitExceptionCode.SUCCESS);
  }

}
