package com.mingshi.skyflying.caffeine;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: MsCaffeine</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/8/1 10:25
 *
 * @Version 1.0
 **/
@Slf4j
@Component
public class MsCaffeine implements ApplicationRunner {

  private static Cache<String/* token */, String/* userName */> tokenUserNameCache = null;
  private static Cache<String/* globalTraceId */, String/* userName */> globalTraceIdUserNameCache = null;
  private static Cache<String/* globalTraceId */, String/* token */> globalTraceIdTokenCache = null;

  @Override
  public void run(ApplicationArguments args) throws Exception {
    try {
      log.info("# MsCaffeine.run() # 项目启动，开始初始化Caffeine实例。");
      createAllCaffeine();
      log.info("# MsCaffeine.run() # 项目启动，初始化Caffeine实例完毕。");
    } catch (Exception e) {
      log.error("# MsCaffeine.run() # 项目启动，初始化Caffeine实例时，出现了异常。", e);
    }
  }

  /**
   * <B>方法名称：putUserNameByGlobalTraceId</B>
   * <B>概要说明：globalTraceId对应的用户名放入到缓存中</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年08月01日 11:08:54
   * @Param [token]
   **/
  public static void putUserNameByGlobalTraceId(String globalTraceId, String userName) {
    globalTraceIdUserNameCache.put(globalTraceId, userName);
  }

  /**
   * <B>方法名称：getUserNameByGlobalTraceId</B>
   * <B>概要说明：根据globalTraceId获取用户名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年08月01日 11:08:54
   * @Param [token]
   **/
  public static String getUserNameByGlobalTraceId(String globalTraceId) {
    return globalTraceIdUserNameCache.getIfPresent(globalTraceId);
  }

  /**
   * <B>方法名称：putUserNameByToken</B>
   * <B>概要说明：将token对应的用户名放入到缓存中</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年08月01日 11:08:54
   * @Param [token]
   **/
  public static void putUserNameByToken(String token, String userName) {
    tokenUserNameCache.put(token, userName);
  }

  /**
   * <B>方法名称：getUserNameByToken</B>
   * <B>概要说明：根据token获取用户名</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年08月01日 11:08:54
   * @Param [token]
   **/
  public static String getUserNameByToken(String token) {
    return tokenUserNameCache.getIfPresent(token);
  }

  /**
   * <B>方法名称：initCaffeine</B>
   * <B>概要说明：项目启动，构建Caffeine实例</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 10:08:14
   * @Param []
   **/
  private void createAllCaffeine() {
    createTokenUserNameCaffeine();
    createGlobalTraceIdUserNameCaffeine();
    createGlobalTraceIdTokenCaffeine();
  }

  /**
   * <B>方法名称：initGlobalTraceIdUserNameCaffeine</B>
   * <B>概要说明：构造globalTraceId和userName的Caffeine实例</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 10:08:10
   * @Param []
   **/
  private void createGlobalTraceIdUserNameCaffeine() {
    globalTraceIdUserNameCache = Caffeine.newBuilder()
      // 初始的缓存空间大小
      .initialCapacity(500)
      // 缓存的最大条数
      .maximumSize(5000)
      .expireAfterAccess(3, TimeUnit.DAYS)
      .recordStats()
      //设置缓存的移除通知
      .removalListener(new CaffeineRemovalListener())
      .build();

    // 参数说明：
    // initialCapacity 初始的缓存空间大小
    // maximumSize 缓存的最大条数
    // maximumWeight 缓存的最大权重
    // expireAfterAccess 最后一次写入或访问后，经过固定时间过期
    // expireAfterWrite 最后一次写入后，经过固定时间过期
    // refreshAfterWrite 写入后，经过固定时间过期，下次访问返回旧值并触发刷新
    // weakKeys 打开 key 的弱引用
    // weakValues 打开 value 的弱引用
    // softValues 打开 value 的软引用
    // recordStats 缓存使用统计
    // expireAfterWrite 和 expireAfterAccess 同时存在时，以 expireAfterWrite 为准。
    // weakValues 和 softValues 不可以同时使用。
    // maximumSize 和 maximumWeight 不可以同时使用。
  }

  /**
   * <B>方法名称：createGlobalTraceIdTokenCaffeine</B>
   * <B>概要说明：维护globalTRaceId和token之间的关系</B>
   * @Author zm
   * @Date 2022年08月01日 13:08:56
   * @Param []
   * @return void
   **/
  private void createGlobalTraceIdTokenCaffeine() {
    globalTraceIdTokenCache = Caffeine.newBuilder()
      // 初始的缓存空间大小
      .initialCapacity(500)
      // 缓存的最大条数
      .maximumSize(5000)
      .expireAfterAccess(3, TimeUnit.DAYS)
      .recordStats()
      //设置缓存的移除通知
      .removalListener(new CaffeineRemovalListener())
      .build();

    // 参数说明：
    // initialCapacity 初始的缓存空间大小
    // maximumSize 缓存的最大条数
    // maximumWeight 缓存的最大权重
    // expireAfterAccess 最后一次写入或访问后，经过固定时间过期
    // expireAfterWrite 最后一次写入后，经过固定时间过期
    // refreshAfterWrite 写入后，经过固定时间过期，下次访问返回旧值并触发刷新
    // weakKeys 打开 key 的弱引用
    // weakValues 打开 value 的弱引用
    // softValues 打开 value 的软引用
    // recordStats 缓存使用统计
    // expireAfterWrite 和 expireAfterAccess 同时存在时，以 expireAfterWrite 为准。
    // weakValues 和 softValues 不可以同时使用。
    // maximumSize 和 maximumWeight 不可以同时使用。
  }

  /**
   * <B>方法名称：initTokenUserNameCaffeine</B>
   * <B>概要说明：构造token和userName的Caffeine实例</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年08月01日 10:08:14
   * @Param []
   **/
  private void createTokenUserNameCaffeine() {
    tokenUserNameCache = Caffeine.newBuilder()
      // 初始的缓存空间大小
      .initialCapacity(100)
      // 缓存的最大条数
      .maximumSize(500)
      .expireAfterAccess(7, TimeUnit.DAYS)
      .recordStats()
      //设置缓存的移除通知
      .removalListener(new CaffeineRemovalListener())
      .build();

    // 参数说明：
    // initialCapacity 初始的缓存空间大小
    // maximumSize 缓存的最大条数
    // maximumWeight 缓存的最大权重
    // expireAfterAccess 最后一次写入或访问后，经过固定时间过期
    // expireAfterWrite 最后一次写入后，经过固定时间过期
    // refreshAfterWrite 写入后，经过固定时间过期，下次访问返回旧值并触发刷新
    // weakKeys 打开 key 的弱引用
    // weakValues 打开 value 的弱引用
    // softValues 打开 value 的软引用
    // recordStats 缓存使用统计
    // expireAfterWrite 和 expireAfterAccess 同时存在时，以 expireAfterWrite 为准。
    // weakValues 和 softValues 不可以同时使用。
    // maximumSize 和 maximumWeight 不可以同时使用。
  }
}
