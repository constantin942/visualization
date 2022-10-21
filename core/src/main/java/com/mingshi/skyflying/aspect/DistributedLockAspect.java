package com.mingshi.skyflying.aspect;

import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Slf4j
@Aspect
@Component
public class DistributedLockAspect {

  @Autowired
  private RedissonClient redissonClient;

  private final String executeExpr = "execution(* com.mingshi.skyflying.task.ExecitonScheduledTaskList.*(..))";

  @Around(executeExpr)
  public Object doAround(ProceedingJoinPoint pjp) throws Throwable {
    return doLock(pjp);
  }

  private Object doLock(ProceedingJoinPoint pjp) throws Throwable {
    //使用了注解的方法
    String methodName = pjp.getSignature().getName();
    Object[] arguments = pjp.getArgs();
    // 根据方法反射获取想要的锁名字
    String lockName = String.valueOf(arguments[0]);

    boolean getLock = false;
    /**拿到锁的对象*/
    RLock redissonLock = redissonClient.getLock(lockName);
    try {
      /**a. tryLock()方法没有设置参数，使用默认值：获取锁的时间最长是0秒，key的有效期是30秒；
       *  当业务执行时间超过了锁的默认时间30秒，就自动延续时间，可放心使用；
       *  注：不要使用tryLock()方法时，私自设置有效期，否则不会自动延续时间；
       * */
        getLock = redissonLock.tryLock();
        if (Boolean.TRUE.equals(getLock)) {
        return pjp.proceed();
      } else {
        /**没有获取到分布式锁*/
        log.info("# DistributedLockAspect.doLock() # 该实例没有获取到分布式锁，要执行的定时任务是 = 【{}】，锁名称 = 【{}】。", methodName, lockName);
      }
    } catch (Exception e) {
      log.error("# DistributedLockAspect.doLock() # 该实例获取到分布式锁，在执行定时任务（【{}】，锁名称 = 【{}】）时。出现了异常。", methodName, lockName, e);
    } finally {
      if (false != getLock) {
        /**对已经获取到的分布式锁释放掉*/
        log.info("# DistributedLockAspect.doLock() # 该实例获取到分布式锁，执行定时任务完毕（【{}】，锁名称 = 【{}】）。释放分布式锁。", methodName, lockName);
        redissonLock.unlock();
      }
    }
    return null;
  }

  @AfterThrowing(value = executeExpr, throwing = "ex")
  public void afterThrowing(Throwable ex) {
    throw new RuntimeException(ex);
  }

}
