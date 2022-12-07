package com.aiit.skyflying.common.utils;

import com.aiit.skyflying.common.constant.Const;
import com.netflix.hystrix.contrib.javanica.annotation.HystrixCommand;
import com.netflix.hystrix.contrib.javanica.annotation.HystrixProperty;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.StringRedisConnection;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class RedisPoolUtil {

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    /**
     * <B>方法名称：hsetBatch</B>
     * <B>概要说明：向一张hash表中批量放入数据,如果不存在将创建</B>
     *
     * @return boolean
     * @Author zm
     * @Date 2022年06月27日 13:06:37
     * @Param [key, map]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    /**
                     * 命令执行超时时间300毫秒
                     */
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmsetFallback")
    public boolean hsetBatch(String key, Map<String, String> map) {
        try {
            stringRedisTemplate.opsForHash().putAll(key, map);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param map
     * @param throwable
     * @return
     */
    public boolean hmsetFallbackBatch(String key, Map<String, String> map, Throwable throwable) {
        log.error("hmsetFallbackBatch 走降级策略啦。降级原因=【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * <B>方法名称：hsetIncrBy</B>
     * <B>概要说明：对哈希表中的某个元素自增指定的数值</B>
     *
     * @return boolean
     * @Author zm
     * @Date 2022年07月18日 16:07:37
     * @Param [key, value, incrementCount]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    /**
                     * 命令执行超时时间300毫秒
                     */
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hsetIncrByFallback")
    public boolean hsetIncrBy(String key, String value, Long incrementCount) {
        try {
            stringRedisTemplate.opsForHash().increment(key, value, incrementCount);
            return true;
        } catch (Exception e) {
            log.error("# RedisPoolUtil.hsetIncrBy # 对key = 【{}】，value = 【{}】进行自增 = 【{}】时，出现了异常。", key, value, incrementCount);
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param value
     * @param incrementCount
     * @param throwable
     * @return
     */
    public boolean hsetIncrByFallback(String key, String value, Long incrementCount, Throwable throwable) {
        log.error("hsetIncrByFallback 走降级策略啦。降级原因=【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    /**
                     * 命令执行超时时间300毫秒
                     */
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmsetFallback2")
    public boolean hsetBatch2(String key, Map<String, Integer> map) {
        try {
            stringRedisTemplate.opsForHash().putAll(key, map);
            return true;
        } catch (Exception e) {
            log.error("# RedisPoolUtil.hsetBatch2() # 往redis中存放数据时，出现了异常。", e);
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param map
     * @param throwable
     * @return
     */
    public boolean hmsetFallbackBatch2(String key, Map<String, Integer> map, Throwable throwable) {
        log.error("hmsetFallbackBatch2 走降级策略啦。降级原因=【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    //批量插入redis，key的类型是string
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "insertRedisStringSetBatchFallback")
    public List<Object> insertRedisStringSetBatch(final Map<String, String> map) {
        List<Object> results = stringRedisTemplate.executePipelined(new RedisCallback<String>() {
            @Override
            public String doInRedis(RedisConnection connection) throws DataAccessException {
                StringRedisConnection stringRedisConn = (StringRedisConnection) connection;
                for (String key : map.keySet()) {
                    String value = String.valueOf(map.get(key));
                    stringRedisConn.set(key, value);//批量String
                    // stringRedisConn.expire(key.getBytes(), 200);//过期时间单位秒，如果不设置，就是永久有效；但不能人为设置为-1；
                }
                return null;
            }
        });
        return results;
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param map
     * @param throwable
     * @return
     */
    public List<Object> insertRedisStringSetBatchFallback(Map<String, String> map, Throwable throwable) {
        log.error("insertRedisStringSetBatchFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    //批量插入redis，key的类型是string
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "insertRedisStringPipelineMsetBatchFallback")
    public List<Object> insertRedisStringPipelineMsetBatch(final Map<String, String> map) {
        List<Object> results = stringRedisTemplate.executePipelined(new RedisCallback<String>() {
            @Override
            public String doInRedis(RedisConnection connection) throws DataAccessException {
                StringRedisConnection stringRedisConn = (StringRedisConnection) connection;
                Map<byte[], byte[]> tuple = new HashMap<>(Const.NUMBER_EIGHT);
                for (String key : map.keySet()) {
                    String value = String.valueOf(map.get(key));
                    tuple.put(key.getBytes(), value.getBytes());
                }
                stringRedisConn.mSet(tuple);
                return null;
            }
        });
        return results;
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param map
     * @param throwable
     * @return
     */
    public List<Object> insertRedisStringPipelineMsetBatchFallback(Map<String, String> map, Throwable throwable) {
        log.error("insertRedisStringPipelineMsetBatchFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @return java.lang.String
     * @Author zhaoming
     * @Description 批量查询，一次性获取10万条数据，约8秒钟，很快；
     * @Date 13:50 2020/8/11
     * @Param []
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "mgetFallback")
    public List<String> mget(List<String> keys) {
        return stringRedisTemplate.opsForValue().multiGet(keys);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param keys
     * @param throwable
     * @return
     */
    public List<String> mgetFallback(List<String> keys, Throwable throwable) {
        log.error("mgetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @return java.lang.String
     * @Author zhaoming
     * @Description 批量插入，一次性插入10万条数据，约8秒钟，很快；
     * @Date 13:50 2020/8/11
     * @Param []
     **/
    //批量插入redis，key的类型是string
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "mSetFallback")
    public void mSet(final Map<String, String> map) {
        stringRedisTemplate.opsForValue().multiSet(map);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param map
     * @param throwable
     */
    public void mSetFallback(Map<String, String> map, Throwable throwable) {
        log.error("mSetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return;
    }

    /**
     * 为哈希表 key 中的指定字段的整数值加上增量 increment
     *
     * @param key
     * @param field
     * @param increment
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hIncrByFallback")
    public Long hIncrBy(String key, Object field, long increment) {
        return stringRedisTemplate.opsForHash().increment(key, field, increment);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param field
     * @param increment
     * @param throwable
     * @return
     */
    public Long hIncrByFallback(String key, Object field, long increment, Throwable throwable) {
        log.error("hIncrByFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * 为哈希表 key 中的指定字段的整数值加上增量 increment
     *
     * @param key
     * @param field
     * @param delta
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hIncrByFloatFallback")
    public Double hIncrByFloat(String key, Object field, double delta) {
        return stringRedisTemplate.opsForHash().increment(key, field, delta);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param field
     * @param delta
     * @param throwable
     * @return
     */
    public Double hIncrByFloatFallback(String key, Object field, double delta, Throwable throwable) {
        log.error("hIncrByFloatFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @return java.lang.String
     * @Author zhaoming
     * @Description 批量查询：一次性获取10万条数据，约42秒钟，较慢；
     * @Date 13:50 2020/8/11
     * @Param []
     **/
    //批量获取redis，key的类型是string
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "queryRedisStringBatchFallback")
    public List<Object> queryRedisStringBatch(final List<String> queryList) {
        List<Object> results = stringRedisTemplate.executePipelined(
                new RedisCallback<Object>() {
                    @Override
                    public Object doInRedis(RedisConnection connection) throws DataAccessException {
                        StringRedisConnection stringRedisConn = (StringRedisConnection) connection;
                        for (int i = 0; i < queryList.size(); i++) {
                            String key = queryList.get(i);
                            stringRedisConn.get(key);
                        }
                        return null;
                    }
                });
        return results;
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param queryList
     * @param throwable
     * @return
     */
    public List<Object> queryRedisStringBatchFallback(List<String> queryList, Throwable throwable) {
        log.error("queryRedisStringBatchFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * 指定缓存失效时间
     *
     * @param key  键
     * @param time 时间(秒)
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "expireFallback")
    public boolean expire(String key, long time) {
        try {
            if (time > 0) {
                stringRedisTemplate.expire(key, time, TimeUnit.SECONDS);
            }
            return true;
        } catch (Exception e) {
            log.error("设置key = 【{}】的有效期 = 【{}】时，出现了异常。", key, time, e);
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param time
     * @param throwable
     * @return
     */
    public boolean expireFallback(String key, long time, Throwable throwable) {
        log.error("expireFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * 根据key 获取过期时间
     *
     * @param key 键 不能为null
     * @return 时间(秒) 返回0代表为永久有效
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "getExpireFallback")
    public long getExpire(String key) {
        return stringRedisTemplate.getExpire(key, TimeUnit.SECONDS);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param throwable
     * @return
     */
    public long getExpireFallback(String key, Throwable throwable) {
        log.error("getExpireFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1;
    }

    /**
     * 判断key是否存在
     *
     * @param key 键
     * @return true 存在 false不存在
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hasKeyFallback")
    public boolean hasKey(String key) {
        try {
            return stringRedisTemplate.hasKey(key);
        } catch (Exception e) {
            e.printStackTrace();
            log.error("在Redis中判断key = 【{}】是否存在时，出现了异常。", key, e);
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param throwable
     * @return
     */
    public boolean hasKeyFallback(String key, Throwable throwable) {
        log.error("hasKeyFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * 删除缓存
     *
     * @param key 可以传一个值 或多个
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "getFallback")
    public void del(String... key) {
        if (key != null && key.length > 0) {
            if (key.length == 1) {
                stringRedisTemplate.delete(key[0]);
            } else {
                stringRedisTemplate.delete(CollectionUtils.arrayToList(key));
            }
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     */
    public void delFallback(String... key) {
        log.error("delFallback 走降级策略啦。");
        return;
    }

    /**
     * 批量删除缓存
     *
     * @param list
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "delBatchFallback")
    public void delBatch(List<String> list) {
        if (null != list && list.size() > 0) {
            stringRedisTemplate.delete(list);
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param list
     */
    public void delBatchFallback(List<String> list) {
        log.error("delBatchFallback 走降级策略啦。");
        return;
    }

    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "delFallback")
    public boolean del(String key) {
        try {
            return stringRedisTemplate.delete(key);
        } catch (Exception e) {
            log.error("根据key = 【{}】在Redis中删除数据时，出现了异常。", key);
        }
        return false;
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param throwable
     * @return
     */
    public boolean delFallback(String key, Throwable throwable) {
        log.error("delFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }
    //============================String=============================

    /**
     * 普通缓存获取
     *
     * @param key 键
     * @return 值
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "getFallback")
    public Object get(String key) {
        try {
            return key == null ? null : stringRedisTemplate.opsForValue().get(key);
        } catch (Exception e) {
            log.error("根据key = 【{}】在Redis中获取数据时，出现了异常。", key, e);
        }
        return null;
    }


    /**
     * 模糊key获取
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "getFallback")
    public Map<String, String> fuzGet(String prefix) {
        try {
            Set<String> keySet = stringRedisTemplate.keys(prefix + "*");
            if (keySet != null) {
                List<String> values = stringRedisTemplate.opsForValue().multiGet(keySet);
                List<String> keys = new ArrayList<>(keySet);
                HashMap<String, String> res = new HashMap<>(Const.NUMBER_EIGHT);
                for(int i = 0; i < keys.size(); i++) {
                    res.put(keys.get(i), values.get(i));
                }
                return res;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param throwable
     * @return
     */
    public Object getFallback(String key, Throwable throwable) {
        log.error("getFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * 普通缓存放入
     *
     * @param key   键
     * @param value 值
     * @return true成功 false失败
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "setFallback")
    public boolean set(String key, Object value) {
        try {
            stringRedisTemplate.opsForValue().set(key, String.valueOf(value));
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            log.error("将key = 【{}】对应的value = 【{}】插入到Redis中出现了异常。", key, value, e);
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param value
     * @param throwable
     * @return
     */
    public boolean setFallback(String key, Object value, Throwable throwable) {
        log.error("setFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "setNxFallback")
    public boolean setNx(String key, Object value) {
        try {
            return stringRedisTemplate.opsForValue().setIfAbsent(key, String.valueOf(value));
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param value
     * @param throwable
     * @return
     */
    public boolean setNxFallback(String key, Object value, Throwable throwable) {
        log.error("setNxFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * @return boolean
     * @Author zhaoming
     * @Description
     * @Date 15:12 2020/2/14
     * @Param [key, value, time]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "setNxFallback")
    public boolean setNx(String key, Object value, int time) {
        try {
            return stringRedisTemplate.opsForValue().setIfAbsent(key, String.valueOf(value), time, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("将key = 【{}】，value = 【{}】插入到Redis中出现了异常。", key, String.valueOf(value));
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param value
     * @param time
     * @param throwable
     * @return
     */
    public boolean setNxFallback(String key, Object value, int time, Throwable throwable) {
        log.error("setNxFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * 普通缓存放入并设置时间
     *
     * @param key   键
     * @param value 值
     * @param time  时间(秒) time要大于0 如果time小于等于0 将设置无限期
     * @return true成功 false 失败
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "setFallback")
    public boolean set(String key, Object value, long time) {
        try {
            if (time > 0) {
                stringRedisTemplate.opsForValue().set(key, String.valueOf(value), time, TimeUnit.SECONDS);
            } else {
                set(key, value);
            }
            return true;
        } catch (Exception e) {
            log.error("执行RedisPoolUtil的set方法出现了异常", e);
            return false;
        }
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param value
     * @param time
     * @param throwable
     * @return
     */
    public boolean setFallback(String key, Object value, long time, Throwable throwable) {
        log.error("setFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * 递增
     *
     * @param key 键
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "incrFallback")
    public long incr(String key, long delta) {
        if (delta < 0) {
            throw new RuntimeException("递增因子必须大于0");
        }
        return stringRedisTemplate.opsForValue().increment(key, delta);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param delta
     * @param throwable
     * @return
     */
    public long incrFallback(String key, long delta, Throwable throwable) {
        log.error("incrFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1;
    }

    /**
     * 递减
     *
     * @param key 键
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "decrFallback")
    public long decr(String key, long delta) {
        if (delta < 0) {
            throw new RuntimeException("递减因子必须大于0");
        }
        return stringRedisTemplate.opsForValue().increment(key, -delta);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param delta
     * @param throwable
     * @return
     */
    public long decrFallback(String key, long delta, Throwable throwable) {
        log.error("decrFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1;
    }
    //================================Map=================================

    /**
     * HashGet
     *
     * @param key  键 不能为null
     * @param item 项 不能为null
     * @return 值
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hgetFallback")
    public Object hget(String key, String item) {
        return stringRedisTemplate.opsForHash().get(key, item);
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param item
     * @param throwable
     * @return
     */
    public Object hgetFallback(String key, String item, Throwable throwable) {
        log.error("hgetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * <B>方法名称：hgetKeys</B>
     * <B>概要说明：获取hash表中所有的key</B>
     *
     * @return java.lang.Object
     * @Author zm
     * @Date 2022年07月20日 16:07:57
     * @Param [key, item]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hgetKeysFallback")
    public Set<Object> hgetKeys(String key) {
        Set<Object> keys = stringRedisTemplate.opsForHash().keys(key);
        return keys;
    }

    /**
     * 当调用Redis缓存时，若是出现异常，则自动调用降级方法
     *
     * @param key
     * @param throwable
     * @return
     */
    public Object hgetKeysFallback(String key, Throwable throwable) {
        log.error("hgetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * <B>方法名称：hgetSize</B>
     * <B>概要说明：获取哈希集合中元素的个数</B>
     *
     * @return java.lang.Long
     * @Author zm
     * @Date 2022年07月21日 09:07:48
     * @Param [key]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hgetSizeFallback")
    public Long hgetSize(String key) {
        return stringRedisTemplate.opsForHash().size(key);
    }

    public Object hgetSizeFallback(String key, Throwable throwable) {
        log.error("hgetSizeFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * 获取hashKey对应的所有键值
     *
     * @param key 键
     * @return 对应的多个键值
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmgetFallback")
    public Map<Object, Object> hmget(String key) {
        return stringRedisTemplate.opsForHash().entries(key);
    }

    public Map<Object, Object> hmgetFallback(String key, Throwable throwable) {
        log.error("hmgetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * HashSet
     *
     * @param key 键
     * @param map 对应多个键值
     * @return true 成功 false 失败
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmsetFallback")
    public boolean hmset(String key, Map<String, Object> map) {
        try {
            stringRedisTemplate.opsForHash().putAll(key, map);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean hmsetFallback(String key, Map<String, Object> map, Throwable throwable) {
        log.error("hmsetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * HashSet 并设置时间
     *
     * @param key  键
     * @param map  对应多个键值
     * @param time 时间(秒)
     * @return true成功 false失败
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmsetFallback")
    public boolean hmset(String key, Map<String, Object> map, long time) {
        try {
            stringRedisTemplate.opsForHash().putAll(key, map);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean hmsetFallback(String key, Map<String, Object> map, long time, Throwable throwable) {
        log.error("hmsetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * 向一张hash表中放入数据,如果不存在将创建
     *
     * @param key   键
     * @param item  项
     * @param value 值
     * @return true 成功 false失败
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmsetFallback")
    public boolean hset(String key, String item, Object value) {
        try {
            stringRedisTemplate.opsForHash().put(key, item, value);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean hmsetFallback(String key, String item, Object value, Throwable throwable) {
        log.error("hmsetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * @Author zhaoming
     * @Description 往redis的set集合中，放入数据；
     * @Date 10:53 2020/12/11
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "saddFallback")
    public Long sadd(String setName, String value) {
        try {
            return stringRedisTemplate.opsForSet().add(setName, value);
        } catch (Exception e) {
            e.printStackTrace();
            return -1L;
        }
    }

    public Long saddFallback(String setName, String value, Throwable throwable) {
        log.error("saddFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @Author zhaoming
     * @Description 往redis的set集合中，放入数据；
     * @Date 10:53 2020/12/11
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "setSizeFallback")
    public Long setSize(String setName) {
        try {
            Long size = stringRedisTemplate.opsForSet().size(setName);
            return size;
        } catch (Exception e) {
            log.error("# RedisPoolUtil.setSize() # 从Redis的set集合中获取元素个数时，出现了异常。", e);
            return -1L;
        }
    }

    public Long setSizeFallback(String setName, Throwable throwable) {
        log.error("setSizeFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @Author zhaoming
     * @Description 从redis的set集合中，获取所有的数据；
     * @Date 10:53 2020/12/11
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "smembersFallback")
    public Set<String> smembers(String setName) {
        try {
            return stringRedisTemplate.opsForSet().members(setName);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public Set<String> smembersFallback(String setName, Throwable throwable) {
        log.error("smembersFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @Author zhaoming
     * @Description 从redis的set集合中，获取所有的数据；
     * @Date 10:53 2020/12/11
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "isMemberFallback")
    public Boolean isMember(String key, String member) {
        try {
            return stringRedisTemplate.opsForSet().isMember(key, member);
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public Boolean isMemberFallback(String key, String member, Throwable throwable) {
        log.error("isMemberFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * <B>方法名称：hDelete</B>
     * <B>概要说明：从一个哈希结构中，删除指定的元素；</B>
     *
     * @return java.lang.Long
     * @Author zm
     * @Date 2022年06月27日 14:06:25
     * @Param [key, params]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hDeleteFallback")
    public Long hDelete(String key, Object... params) {
        try {
            return stringRedisTemplate.opsForHash().delete(key, params);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public Long hDeleteFallback(String key, Throwable throwable) {
        log.error("hDeleteFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * <B>方法名称：hgetall</B>
     * <B>概要说明：从一个哈希结构中，获取所有的key和value；</B>
     *
     * @return java.util.Map<java.lang.Object, java.lang.Object>
     * @Author zm
     * @Date 2022年06月27日 14:06:07
     * @Param [key]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hgetallFallback")
    public Map<Object, Object> hgetall(String key) {
        try {
            return stringRedisTemplate.opsForHash().entries(key);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public Map<Object, Object> hgetallFallback(String key, Throwable throwable) {
        log.error("hgetallFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * @return boolean
     * @Author zhaoming
     * @Description 从一个哈希结构中，获取所有的key；
     * @Date 10:06 2020/12/11
     * @Param [key]
     **/
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hgetallKeysFallback")
    public Set<Object> hgetallKeys(String key) {
        try {
            return stringRedisTemplate.opsForHash().keys(key);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public Set<Object> hgetallKeysFallback(String key, Throwable throwable) {
        log.error("hgetallFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hmgetFallback")
    public List<Object> hmget(String key, List<Object> set) {
        try {
            return stringRedisTemplate.opsForHash().multiGet(key, set);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public List<Object> hmgetFallback(String key, List<Object> set, Throwable throwable) {
        log.error("hmgetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * 向一张hash表中放入数据,如果不存在将创建
     *
     * @param key   键
     * @param item  项
     * @param value 值
     * @param time  时间(秒)  注意:如果已存在的hash表有时间,这里将会替换原有的时间
     * @return true 成功 false失败
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hsetFallback")
    public boolean hset(String key, String item, Object value, long time) {
        try {
            stringRedisTemplate.opsForHash().put(key, item, value);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public boolean hsetFallback(String key, String item, Object value, long time, Throwable throwable) {
        log.error("hsetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * 删除hash表中的值
     *
     * @param key  键 不能为null
     * @param item 项 可以使多个 不能为null
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hdelFallback")
    public void hdel(String key, Object... item) {
        stringRedisTemplate.opsForHash().delete(key, item);
    }

    public void hdelFallback(String key, Object... item) {
        log.error("hdelFallback 走降级策略啦。");
        return;
    }

    /**
     * 判断hash表中是否有该项的值
     *
     * @param key  键 不能为null
     * @param item 项 不能为null
     * @return true 存在 false不存在
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hHasKeyFallback")
    public boolean hHasKey(String key, String item) {
        return stringRedisTemplate.opsForHash().hasKey(key, item);
    }

    public boolean hHasKeyFallback(String key, String item, Throwable throwable) {
        log.error("hHasKeyFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return false;
    }

    /**
     * hash递增 如果不存在,就会创建一个 并把新增后的值返回
     *
     * @param key  键
     * @param item 项
     * @param by   要增加几(大于0)
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hincrFallback")
    public double hincr(String key, String item, double by) {
        return stringRedisTemplate.opsForHash().increment(key, item, by);
    }

    public double hincrFallback(String key, String item, double by, Throwable throwable) {
        log.error("hincrFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1;
    }

    /**
     * hash递减
     *
     * @param key  键
     * @param item 项
     * @param by   要减少记(小于0)
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "hdecrFallback")
    public double hdecr(String key, String item, double by) {
        return stringRedisTemplate.opsForHash().increment(key, item, -by);
    }

    public double hdecrFallback(String key, String item, double by, Throwable throwable) {
        log.error("hdecrFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1;
    }

    /**
     * Sorted set :有序集合添加
     *
     * @param key
     * @param value
     * @param scoure
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "zAddFallback")
    public Boolean zAdd(String key, String value, Double scoure) {
        try {
            ZSetOperations<String, String> stringStringZsetOperations = stringRedisTemplate.opsForZSet();
            return stringStringZsetOperations.add(key, value, scoure);
        } catch (Exception e) {
            log.error("# RedisPoolUtil.zAdd() # 往有序集合中存放数据时，出现了异常。", e);
            return false;
        }
    }

    public double zAddFallback(String key, String value, double scoure, Throwable throwable) {
        log.error("zAddFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1;
    }

    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "zAddBatchFallback")
    public Long zAddBatch(String key, Set<ZSetOperations.TypedTuple<String>> tuples) {
        try {
            ZSetOperations<String, String> stringStringZsetOperations = stringRedisTemplate.opsForZSet();
            return stringStringZsetOperations.add(key, tuples);
        } catch (Exception e) {
            log.error("# RedisPoolUtil.zAddBatch() # 往有序集合中存放数据时，出现了异常。", e);
            return -1L;
        }
    }

    public Long zAddBatchFallback(String key, Set<ZSetOperations.TypedTuple<String>> tuples, Throwable throwable) {
        log.error("zAddBatchFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return -1L;
    }

    /**
     * Sorted set :将有序集合中的某个元素进行累加；
     *
     * @param key
     * @param value
     * @param scoure
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },
            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "incrementScoreFallback")
    public void zSetIncrementScore(String key, String value, double scoure) {
        try {
            ZSetOperations<String, String> stringStringZsetOperations = stringRedisTemplate.opsForZSet();
            stringStringZsetOperations.incrementScore(key, value, scoure);
        } catch (Exception e) {
            log.error("将有序集合中的某个元素进行累时，出现了异常。", e);
        }
    }

    public void incrementScoreFallback(String key, String value, double scoure, Throwable throwable) {
        log.error("incrementScoreFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return;
    }

    /**
     * <B>方法名称：incrementScoreByBatch</B>
     * <B>概要说明：批量插入到Redis的Zset集合中</B>
     *
     * @return void
     * @Author zm
     * @Date 2022年07月25日 18:07:47
     * @Param [list, redisCacheKey]
     **/
    public void incrementScoreByBatch(String redisCacheKey, List<HashMap<String, Double>> list) {
        Set<ZSetOperations.TypedTuple<String>> dataSet = new HashSet<>();
        for (HashMap<String, Double> data : list) {
            Iterator<String> iterator = data.keySet().iterator();
            while (iterator.hasNext()) {
                String key = iterator.next();
                Double value = data.get(key);
                ZSetOperations.TypedTuple<String> typedTuple = new DefaultTypedTuple<>(key, value);
                dataSet.add(typedTuple);
            }
        }
        if (null != dataSet && dataSet.size() > 0) {
            stringRedisTemplate.opsForZSet().add(redisCacheKey, dataSet);
        }
    }

    /**
     * Sorted set:有序集合获取
     *
     * @param key
     * @param scoure
     * @param scoure1
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "reverseRangeFallback")
    public Set<String> reverseRange(String key, Long scoure, Long scoure1) {
        try {
            ZSetOperations<String, String> zset = stringRedisTemplate.opsForZSet();
            return zset.reverseRange(key, scoure, scoure1);
        } catch (Exception e) {
            log.error("从有序集合中获取元素时，出现了异常。", e);
        }
        return null;
    }

    public Set<String> reverseRangeFallback(String key, Long scoure, Long scoure1, Throwable throwable) {
        log.error("reverseRangeFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * Sorted set:从有序集合中获取元素的个数；2021-05-18 10:38:53
     *
     * @param key
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "sizeFromZsetFallback")
    public Long sizeFromZset(String key) {
        try {
            ZSetOperations<String, String> zset = stringRedisTemplate.opsForZSet();
            return zset.size(key);
        } catch (Exception e) {
            log.error("从有序集合中获取元素的个数时，出现了异常。", e);
        }
        return null;
    }

    public Long sizeFromZsetFallback(String key, Throwable throwable) {
        log.error("sizeFromZsetFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }

    /**
     * Sorted set:根据排名，从低到高在有序集合中获取一部分元素；
     *
     * @param key
     * @param scoure
     * @param scoure1
     * @return
     */
    @HystrixCommand(
            threadPoolProperties = {
                    @HystrixProperty(name = "coreSize", value = "10"),// 线程池中最多有10个线程
                    @HystrixProperty(name = "maxQueueSize", value = "1500"),
                    @HystrixProperty(name = "queueSizeRejectionThreshold", value = "1000"),
            },

            commandProperties = {
                    //命令执行超时时间300毫秒
                    @HystrixProperty(name = "execution.isolation.thread.timeoutInMilliseconds", value = "1000"),
            }, fallbackMethod = "reverseRangeWithScoresFallback")
    public Set<ZSetOperations.TypedTuple<String>> reverseRangeWithScores(String key, Long scoure, Long scoure1) {
        try {
            ZSetOperations<String, String> zset = stringRedisTemplate.opsForZSet();
            return zset.reverseRangeWithScores(key, scoure, scoure1);
        } catch (Exception e) {
            log.error("根据排名，从低到高在有序集合中获取一部分元素。", e);
        }
        return null;
    }

    public Set<ZSetOperations.TypedTuple<String>> reverseRangeWithScoresFallback(String key, Long scoure, Long scoure1, Throwable throwable) {
        log.error("reverseRangeWithScoresFallback 走降级策略啦。降级原因 = 【{}】【{}】【{}】。", throwable.getMessage(), throwable.getCause(), throwable.getStackTrace());
        return null;
    }
}
