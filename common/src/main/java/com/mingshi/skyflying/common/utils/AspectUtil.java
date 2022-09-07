package com.mingshi.skyflying.common.utils;

import com.mingshi.skyflying.common.response.ServerResponse;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * @ClassName AspectUtil
 *
 * Author apple
 * Date 2020/2/2 16:09
 * @Version 1.0
 **/
@Slf4j
@Component
public class AspectUtil {

  // 方法执行超时时间
  private final long maxReduceTime=1000;


  public ServerResponse<String> excute(ProceedingJoinPoint joinPoint) {
    ServerResponse<String> resObj=null;
    try {
      //执行原方法
      resObj=(ServerResponse<String>) joinPoint.proceed();
    } catch (Exception e) {
      log.error("方法执行异常!", e);
    } catch (Throwable throwable) {
      throwable.printStackTrace();
    }
    return resObj;
  }

  /**
   * @return void
   * @Author zhaoming
   * @Description 获取请求日志
   * @Date 16:13 2020/1/31
   * @Param [joinPoint]
   **/
  public Map<String, Object> getRequestLog(JoinPoint joinPoint) {
    //获取到请求的属性
    ServletRequestAttributes attributes=(ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
    //获取到请求对象
    HttpServletRequest request=attributes.getRequest();
    Map<String, Object> map=new HashMap<>();
    String methodType=request.getMethod();
    String method=joinPoint.getSignature().getName();
    String className=joinPoint.getSignature().getDeclaringTypeName();
    //URL：根据请求对象拿到访问的地址
    map.put("url", request.getRequestURL());

    String oldPassword=request.getParameter("oldPassword");
    String newPassword=request.getParameter("newPassword");
    String userName=request.getParameter("userName");
    String addUserName=request.getParameter("addUserName");
    String addPassword=request.getParameter("addPassword");
    String addPhone=request.getParameter("addPhone");
    if(null != request.getParameter("roleId")){
      Integer roleId=Integer.valueOf(request.getParameter("roleId"));
      map.put("roleId", roleId);
    }
    map.put("oldPassword", oldPassword);
    map.put("newPassword", newPassword);
    map.put("userName", userName);
    map.put("addUserName", addUserName);
    map.put("addPassword", addPassword);
    map.put("addPhone", addPhone);
    map.put("methodType", methodType);
    map.put("method", method);
    //ip：获取到访问
    map.put("ip", request.getRemoteAddr());
    //获取被拦截的类名和方法名
    map.put("class.method", className + "." + method);
    map.put("rqsTime", DateUtil.formatWithDateTimeShort(new Date()));

    return map;
  }

  public ServerResponse<String> excute(ProceedingJoinPoint joinPoint, String phone) {
    ServerResponse<String> resObj=null;
    Object[] args=null;
    try {
      args=joinPoint.getArgs();
      // if(0 < args.length){
        args[args.length - 1]=phone;
      // }
      //执行原方法
      resObj=(ServerResponse<String>) joinPoint.proceed(args);
    } catch (Exception e) {
      log.error("方法执行异常!", e);
    } catch (Throwable throwable) {
      throwable.printStackTrace();
    }
    return resObj;
  }

  public ServerResponse<String> excute(ProceedingJoinPoint joinPoint,String oldPassword,String newPassword, String userName) {
    ServerResponse<String> resObj=null;
    try {
      Object[] args=joinPoint.getArgs();
      args[0]=oldPassword;
      args[1]=newPassword;
      args[2]=userName;
      //执行原方法
      resObj=(ServerResponse<String>) joinPoint.proceed(args);
    } catch (Exception e) {
      log.error("方法执行异常!", e);
    } catch (Throwable throwable) {
      throwable.printStackTrace();
    }
    return resObj;
  }

  public ServerResponse<String> excute(ProceedingJoinPoint joinPoint,String creator, String addUserName,String addPassword,String addPhone,Integer ruleId, HttpServletRequest request) {
    ServerResponse<String> resObj=null;
    try {
      Object[] args=joinPoint.getArgs();
      args[0]=addUserName;
      args[1]=addPassword;
      args[2]=addPhone;
      args[3]=ruleId;
      args[4]=creator;
      args[5]=request;
      //执行原方法
      resObj=(ServerResponse<String>) joinPoint.proceed(args);
    } catch (Exception e) {
      log.error("方法执行异常!", e);
    } catch (Throwable throwable) {
      throwable.printStackTrace();
    }
    return resObj;
  }


  public void outputLog(Instant startTime , Map<String, Object> map, String responseInfo, String methodName) {
    // 计算耗时
    Long diffTime=this.getTimeMillis(startTime);

    Map<String, Object> mapTemp=new HashMap<>();
    mapTemp.put("requestInfo", map);
    mapTemp.put("responseInfo", responseInfo);
    String temp="执行接口" + methodName + "所花时间";
    mapTemp.put(temp, diffTime + " ms");
//    log.info("===================接口 " + methodName + " 执行结束===================" + JsonUtil.obj2String(mapTemp));
  }


  /**
   * @return long
   * @Author zhaoming
   * @Description 获取两个时间差的毫秒数
   * @Date 14:51 2020/7/14
   * @Param [instStart]
   **/
  public long getTimeMillis(Instant instStart) {
    Instant instEnd=Instant.now();
    return Duration.between(instStart, instEnd).toMillis();
  }

}
