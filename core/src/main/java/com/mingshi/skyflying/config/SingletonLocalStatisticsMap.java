// package com.mingshi.skyflying.config;
//
// import com.mingshi.skyflying.dao.MsSegmentDetailDao;
// import com.mingshi.skyflying.domain.MsSegmentDetailDo;
// import com.mingshi.skyflying.utils.DateTimeUtil;
// import lombok.extern.slf4j.Slf4j;
// import org.springframework.boot.ApplicationArguments;
// import org.springframework.boot.ApplicationRunner;
// import org.springframework.stereotype.Component;
//
// import javax.annotation.Resource;
// import java.time.Instant;
// import java.util.Date;
// import java.util.List;
// import java.util.Map;
// import java.util.concurrent.ConcurrentHashMap;
// import java.util.concurrent.atomic.AtomicBoolean;
//
// /**
//  * <B>主类名称: SingletonLocalStatisticsMap</B>
//  * <B>概要说明：</B>
//  * Author zm
//  * Date 2022/5/20 13:39
//  *
//  * @Version 1.0
//  **/
// @Slf4j
// @Component
// public class SingletonLocalStatisticsMap implements ApplicationRunner {
//
//   @Resource
//   private MsSegmentDetailDao msSegmentDetailDao;
//
//   private static AtomicBoolean atomicBooleanIsChanged = new AtomicBoolean(false);
//
//   private static AtomicBoolean atomicBooleanIsUpdatingData = new AtomicBoolean(false);
//
//   public static Boolean getAtomicBooleanIsUpdatingData() {
//     return atomicBooleanIsUpdatingData.get();
//   }
//
//   public static void setAtomicBooleanIsUpdatingData(Boolean flag) {
//     atomicBooleanIsUpdatingData.set(flag);
//   }
//
//   public static Boolean getAtomicBooleanIsChanged() {
//     return atomicBooleanIsChanged.get();
//   }
//
//   public static void setAtomicBooleanIsChanged(Boolean flag) {
//     atomicBooleanIsChanged.set(flag);
//   }
//
//   private static Map<String/* token */, String/* userName */> tokenAndUserNameMap = new ConcurrentHashMap<>();
//   private static Map<String/* globalTraceId */, String/* userName */> globalTraceIdAndUserNameMap = new ConcurrentHashMap<>();
//   private static Map<String/* globalTraceId */, String/* token */> globalTraceIdAndTokenMap = new ConcurrentHashMap<>();
//
//   public static Map<String, String> getGlobalTraceIdAndTokenMapMap() {
//     return globalTraceIdAndTokenMap;
//   }
//
//   public static Map<String, String> getTokenAndUserNameMap() {
//     return tokenAndUserNameMap;
//   }
//
//   public static Map<String, String> getGlobalTraceIdAndUserNameMap() {
//     return globalTraceIdAndUserNameMap;
//   }
//
//   public static Integer getTokenAndUserNameMapLength() {
//     return tokenAndUserNameMap.toString().getBytes().length;
//   }
//
//   public static Integer getGlobalTraceIdAndUserNameMapLength() {
//     return globalTraceIdAndUserNameMap.toString().getBytes().length;
//   }
//
//   public static Integer getGlobalTraceIdAndTokenMapLength() {
//     return globalTraceIdAndTokenMap.toString().getBytes().length;
//   }
//
//   /**
//    * <B>方法名称：run</B>
//    * <B>概要说明：项目启动，从数据库中加载用户名、token、globalTraceId</B>
//    * 这么做的意义在于：当系统在正常运行时，在本地内存会保存用户和token的关系。
//    * 当系统关闭后再重新启动时，本地内存中就不再保存原有的用户和token的关系，
//    * 此时再有请求进来且只带有token，那么这些记录将找不到所属用户。
//    * 为了解决这个问题，所以需要在项目启动时，就把用户、token、globalTraceId加载到本地内存中来。
//    *
//    * @return void
//    * @Author zm
//    * @Date 2022年06月08日 14:06:42
//    * @Param [args]
//    **/
//   @Override
//   public void run(ApplicationArguments args) throws Exception {
//     doRun();
//   }
//
//   private void doRun() {
//     Instant now = Instant.now();
//     Date date = DateTimeUtil.removeDays(new Date(), 7);
//     String dateStr = DateTimeUtil.dateToStr(date);
//
//     log.info("# SingletonLocalStatisticsMap.run() # 项目启动，从数据库中加载用户名、token、globalTraceId到本地内存中。");
//
//     // 获取7天之前的用户名、token、globalTraceId；2022-08-01 14:13:24
//     List<MsSegmentDetailDo> list = msSegmentDetailDao.selectByTokenUserNameGlobalTraceIdIsNotNull(dateStr);
//     if (null == list || 0 == list.size()) {
//       return;
//     }
//     for (MsSegmentDetailDo msSegmentDetailDo : list) {
//       String userName = msSegmentDetailDo.getUserName();
//       String token = msSegmentDetailDo.getToken();
//       String globalTraceId = msSegmentDetailDo.getGlobalTraceId();
//       tokenAndUserNameMap.put(token, userName);
//       globalTraceIdAndUserNameMap.put(globalTraceId, userName);
//       globalTraceIdAndTokenMap.put(globalTraceId, token);
//     }
//     Integer tokenAndUserNameMapLength = getTokenAndUserNameMapLength() / 1024 / 1024;
//
//     Integer globalTraceIdAndUserNameMapLength = getGlobalTraceIdAndUserNameMapLength() / 1024 / 1024;
//
//     Integer globalTraceIdAndTokenMapLength = getGlobalTraceIdAndTokenMapLength() / 1024 / 1024;
//     log.info("# SingletonLocalStatisticsMap.run() # 执行完毕，从数据库中加载用户名、token、globalTraceId到本地内存中。token和用户名map占用【{}】MB、全局链路id和用户名map占据【{}】MB、全局链路id和token map占据【{}】MB", tokenAndUserNameMapLength, globalTraceIdAndUserNameMapLength, globalTraceIdAndTokenMapLength);
//     log.info("# SingletonLocalStatisticsMap.run() # 执行完毕，从数据库中加载用户名、token、globalTraceId到本地内存中【{}条】。耗时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
//   }
// }
