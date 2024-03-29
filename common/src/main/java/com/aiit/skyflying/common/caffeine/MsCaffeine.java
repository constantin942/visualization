package com.aiit.skyflying.common.caffeine;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.dao.MsSegmentDetailDao;
import com.aiit.skyflying.common.domain.MsSegmentDetailDo;
import com.aiit.skyflying.common.utils.DateTimeUtil;
import com.aiit.skyflying.common.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

/**
 * <B>主类名称: MsCaffeine</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * Date 2022/8/1 10:25
 * @Version 1.0
 **/
@Slf4j
@Component
public class MsCaffeine implements ApplicationRunner {
    @Resource
    private MsSegmentDetailDao msSegmentDetailDao;
    @Resource
    private MingshiServerUtil mingshiServerUtil;

    private static volatile Cache<String/* token */, String/* userName */> tokenUserNameCache = null;
    private static volatile Cache<String/* globalTraceId */, String/* userName */> globalTraceIdUserNameCache = null;
    private static volatile Cache<String/* globalTraceId */, String/* token */> globalTraceIdTokenCache = null;
    /**
     * 从数据库中加载用户来源信息到本地内存中；2022-11-25 16:42:58
     */
    private static volatile Cache<String/* user from path */, String/* from 中文 */> userFromCacheByDb = null;
    /**
     * 把从数据库中加载好的用户来源信息转成Map，方便从用户访问详情链路信息中识别用户来源；2022-11-25 16:44:07
     */
    private static volatile ConcurrentMap<@NonNull String, @NonNull String> userFromMap;
    /**
     * 从Redis中获取每一个用户的来源统计信息，供前端用户访问行为和数据访问统计接口使用；2022-11-25 16:44:59
     */
    private static volatile Map<String/* 用户名 */, Map<String/* 用户来源*/, Map<String/* 用户访问时间 年月日 */, Integer/* 访问次数 */>>> userFromVisitedTimesMap = new ConcurrentHashMap<>();

    /**
     * <B>方法名称：setUserFromVisitedTimesMap</B>
     * <B>概要说明：将用户的访问来源次数放入到本地内存中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-25 09:49:27
     * @Param [userName, time, times]
     **/
    public void setUserFromVisitedTimesMap(String userName, String userFrom, String time, Integer times) {
        Map<String/* 用户来源*/, Map<String/* 用户访问时间 年月日 */, Integer/* 访问次数 */>> fromTimeTimesMap = userFromVisitedTimesMap.get(userName);
        if (null == fromTimeTimesMap) {
            fromTimeTimesMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
            userFromVisitedTimesMap.put(userName, fromTimeTimesMap);
            Map<String/* 用户来源*/, Integer/* 访问次数 */> map = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
            map.put(time, times);
            fromTimeTimesMap.put(userFrom, map);
        } else {
            Map<String, Integer> stringIntegerMap = fromTimeTimesMap.get(userFrom);
            if (null == stringIntegerMap) {
                stringIntegerMap = new ConcurrentHashMap<>(Const.NUMBER_EIGHT);
                fromTimeTimesMap.put(userFrom, stringIntegerMap);
            }
            stringIntegerMap.put(time, times);
        }
    }

    public Map<String/* 用户名 */, Map<String/* 用户访问时间 年月日 */, Map<String/* 用户来源*/, Integer/* 访问次数 */>>> getUserFromVisitedTimesMap() {
        return userFromVisitedTimesMap;
    }

    public ConcurrentMap<@NonNull String, @NonNull String> getUserFromMap() {
        return userFromMap;
    }

    @Override
    public void run(ApplicationArguments args) throws Exception {
        try {
            log.info("# MsCaffeine.run() # 项目启动，开始初始化Caffeine实例。");
            createAllCaffeine();
            log.info("# MsCaffeine.run() # 项目启动，初始化Caffeine实例完毕。");
            doRun();
            log.info("# MsCaffeine.run() # 项目启动，将用户名、token、globalTraceId放入到Caffeine实例中完毕。");
        } catch (Exception e) {
            log.error("# MsCaffeine.run() # 项目启动，初始化Caffeine实例时，出现了异常。", e);
        }
    }

    /**
     * <B>方法名称：getUserFromCacheByUserFromPath</B>
     * <B>概要说明：根据用户访问路径，从本地缓存获取用户访问名称</B>
     *
     * @return java.lang.String
     * @Author zm
     * @Date 2022-11-23 17:16:11
     * @Param [key]
     **/
    public String getUserFromCacheByUserFromPath(String key) {
        String value = null;
        try {
            if (null == userFromCacheByDb) {
                try {
                    TimeUnit.SECONDS.sleep(10);
                    log.error(" # MsCaffeine.getUserFromCacheByUserFromPath() # 从本地缓存中根据用户访问路径 = 【{}】获取用户访问名称时，本地缓存不存在，等待10秒中。", key);
                } catch (InterruptedException e) {
                    log.error(" # MsCaffeine.getUserFromCacheByUserFromPath() # 从本地缓存中根据用户访问路径 = 【{}】获取用户访问名称时，出现了异常。", key, e);
                }
            } else {
                value = userFromCacheByDb.getIfPresent(key);
            }
        } catch (Exception e) {
            log.error(" # MsCaffeine.getUserFromCacheByUserFromPath() # 从本地缓存中根据用户访问路径 = 【{}】获取用户访问名称时，出现了异常。", e);
        }
        return value;
    }

    public Cache<String/* from en */, String/* from 中文 */> getUserFromCache() {
        return userFromCacheByDb;
    }

    public void setUserFromMap() {
        userFromMap = userFromCacheByDb.asMap();
    }

    /**
     * <B>方法名称：doRun</B>
     * <B>概要说明：项目启动，从数据库中加载用户名、token、globalTraceId到本地内存Caffeine中</B>
     * 这么做的意义在于：当系统在正常运行时，在本地内存会保存用户和token的关系。
     * 当系统关闭后再重新启动时，本地内存中就不再保存原有的用户和token的关系，
     * 此时再有请求进来且只带有token，那么这些记录将找不到所属用户。
     * 为了解决这个问题，所以需要在项目启动时，就把用户、token、globalTraceId加载到本地内存中来。
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 14:08:10
     * @Param []
     **/
    private void doRun() {
        // 获取7天内的用户名、token、globalTraceId；2022-08-01 14:13:24
        setGlobalTraceIdAndUserNameAndToken();

        // 从数据库中加载用户来源；2022-11-23 16:57:25
        mingshiServerUtil.setUserFromByDb(userFromCacheByDb);
    }


    private void setGlobalTraceIdAndUserNameAndToken() {
        Instant now = Instant.now();
        Date date = DateTimeUtil.removeDays(new Date(), 7);
        String dateStr = DateTimeUtil.dateToStr(date);

        log.info("# MsCaffeine.setGlobalTraceIdAndUserNameAndToken() # 项目启动，从数据库中加载用户名、token、globalTraceId到本地内存Caffeine中。");

        List<MsSegmentDetailDo> list = msSegmentDetailDao.selectByTokenUserNameGlobalTraceIdIsNotNull(dateStr);
        if (list.isEmpty()) {
            return;
        }
        for (MsSegmentDetailDo msSegmentDetailDo : list) {
            String userName = msSegmentDetailDo.getUserName();
            String token = msSegmentDetailDo.getToken();
            String globalTraceId = msSegmentDetailDo.getGlobalTraceId();
            tokenUserNameCache.put(token, userName);
            globalTraceIdUserNameCache.put(globalTraceId, userName);
            globalTraceIdTokenCache.put(globalTraceId, token);
        }
        log.info("# MsCaffeine.setGlobalTraceIdAndUserNameAndToken() # 执行完毕，从数据库中加载用户名、token、globalTraceId到本地内存中【{}条】。耗时 = 【{}】毫秒。", list.size(), DateTimeUtil.getTimeMillis(now));
    }

    /**
     * <B>方法名称：putTokenByGlobalTraceId</B>
     * <B>概要说明：globalTraceId对应的token放入到缓存中</B>
     *
     * @return java.lang.String
     * @Author zm
     * @Date 2022年08月01日 11:08:54
     * @Param [token]
     **/
    public static void putTokenByGlobalTraceId(String globalTraceId, String userName) {
        globalTraceIdTokenCache.put(globalTraceId, userName);
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
     * <B>方法名称：getTokenByGlobalTraceId</B>
     * <B>概要说明：根据globalTraceId获取对应的token</B>
     *
     * @return java.lang.String
     * @Author zm
     * @Date 2022年08月01日 14:08:29
     * @Param [globalTraceId]
     **/
    public static String getTokenByGlobalTraceId(String globalTraceId) {
        return globalTraceIdTokenCache.getIfPresent(globalTraceId);
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
    private static void createAllCaffeine() {
        createTokenUserNameCaffeine();
        createUserFromCacheCaffeine();
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
    private static void createGlobalTraceIdUserNameCaffeine() {
        globalTraceIdUserNameCache = Caffeine.newBuilder()
            // 初始的缓存空间大小
            .initialCapacity(100)
            // 缓存的最大条数
            .maximumSize(5000)
            .expireAfterAccess(1, TimeUnit.DAYS)
            .recordStats()
            //设置缓存的移除通知
            .removalListener(new CaffeineRemovalGlobalTraceIdUserNameListener())
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
     *
     * @return void
     * @Author zm
     * @Date 2022年08月01日 13:08:56
     * @Param []
     **/
    private static void createGlobalTraceIdTokenCaffeine() {
        globalTraceIdTokenCache = Caffeine.newBuilder()
            // 初始的缓存空间大小
            .initialCapacity(100)
            // 缓存的最大条数
            .maximumSize(500)
            .expireAfterAccess(1, TimeUnit.DAYS)
            .recordStats()
            //设置缓存的移除通知
            .removalListener(new CaffeineRemovalGlobalTraceIdTokenListener())
            .build();
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
    private static void createTokenUserNameCaffeine() {
        tokenUserNameCache = Caffeine.newBuilder()
            // 初始的缓存空间大小
            .initialCapacity(100)
            // 缓存的最大条数
            .maximumSize(500)
            .expireAfterAccess(3, TimeUnit.DAYS)
            .recordStats()
            //设置缓存的移除通知
            .removalListener(new CaffeineRemovalTokenUserNameListener())
            .build();
    }

    /**
     * <B>方法名称：createUserFromCacheCaffeine</B>
     * <B>概要说明：从数据库中加载数据，创建用户来源缓存</B>
     *
     * @return void
     * @Author zm
     * @Date 2022-11-23 16:54:52
     * @Param []
     **/
    private static void createUserFromCacheCaffeine() {
        userFromCacheByDb = Caffeine.newBuilder()
            // 初始的缓存空间大小
            .initialCapacity(100)
            // 缓存的最大条数
            .maximumSize(5000)
            .expireAfterAccess(365, TimeUnit.DAYS)
            .recordStats()
            .build();
    }
}
