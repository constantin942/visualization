package com.mingshi.skyflying.statistics;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <B>类名称：InformationOverviewSingleton</B>
 * <B>概要说明：信息概况页面用到的单例</B>
 *
 * @Author zm
 * @Date 2022年07月19日 09:07:09
 * @Param
 * @return
 **/
@Slf4j
@Component
public class InformationOverviewSingleton implements ApplicationRunner {

  @Resource
  private RedisPoolUtil redisPoolUtil;

  private static volatile Map<String, Integer> userCountHashMap = new ConcurrentHashMap();

  @Override
  public void run(ApplicationArguments args) throws Exception {
    doRun();
  }

  private void doRun() {
    // 从Redis缓存中获取所有的用户；
    Set<String> smembers = redisPoolUtil.smembers(Const.SET_DATA_STATISTICS_HOW_MANY_USERS);
    if (null != smembers && !smembers.isEmpty()) {
      for (String smember : smembers) {
        userCountHashMap.put(smember, 1);
      }
    }
  }

  /**
   * <B>方法名称：userIsExisted</B>
   * <B>概要说明：用户是否存在</B>
   *
   * @return java.lang.Integer
   * @Author zm
   * @Date 2022年07月19日 09:07:34
   * @Param [tableName]
   **/
  public static Boolean userIsExisted(String userName) {
    try {
      return userCountHashMap.containsKey(userName);
    } catch (Exception e) {
      log.error("# InformationOverviewSingleton.userIsExisted() # 判断用户信息是否已存在时，出现了异常。", e);
      return false;
    }
  }

  /**
   * <B>方法名称：put</B>
   * <B>概要说明：将用户信息放入到map中</B>
   *
   * @return
   * @Author zm
   * @Date 2022年07月19日 10:07:23
   * @Param
   **/
  public static void put(String key) {
    try {
      userCountHashMap.put(key, 1);
    } catch (Exception e) {
      log.error("# InformationOverviewSingleton.put() # 将用户信息放入到map中时，出现了异常。", e);
    }
  }

}
