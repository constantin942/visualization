package com.mingshi.skyflying.aspect;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.OperationLog;
import com.mingshi.skyflying.common.domain.SysOperator;
import com.mingshi.skyflying.common.exception.AiitExceptionCode;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.AspectUtil;
import com.mingshi.skyflying.common.utils.DateUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.init.UpdateOperationRecord;
import com.mingshi.skyflying.service.OperateLogService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import jodd.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.time.Instant;
import java.util.Date;
import java.util.Map;


/**
 * @ClassName 用户是否登录校验
 * <p>
 * Author apple
 * Date 2020/1/31 13:28
 * @Version 1.0
 **/
@Aspect
@Component
@Slf4j
public class DataAccessAspect {
  @Autowired
  private RedisPoolUtil redisPoolUtil;
  @Autowired
  private AspectUtil aspectUtil;
  @Autowired
  private OperateLogService operateLogService;
  @Autowired
  private MingshiServerUtil mingshiServerUtil;

  private ThreadLocal<String> orderIdThreadLocal = new ThreadLocal<>();

  /**
   * 对类ServerlessCheckController中所有方法调用之前，进行登录校验；这种方式不需要在类的头部加注解
   */
  private final String executeExpr = "execution(* com.mingshi.web.controller.SkyflyingController.*(..))";

  /**
   * 只对加了@AspectAnnotation注解的方法进行拦截；
   */
  @Pointcut("@annotation(com.mingshi.skyflying.aspect.AspectAnnotation)")
  private void annotationPointCut() {
  }

  /**
   * @return java.lang.Object
   * @Author zhaoming
   * @Description 环绕通知，拦截controller，输出请求参数、响应内容和响应时间
   * @Date 16:01 2020/1/31
   * @Param [joinPoint]
   **/
  @Around("annotationPointCut()")
  // @Around(executeExpr)
  public Object processLog(ProceedingJoinPoint joinPoint) {
    Instant instStart = Instant.now();
    //获取到请求的属性
    ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
    //获取到请求对象
    HttpServletRequest request = attributes.getRequest();

    ServerResponse<String> resObj = null;
    String userName = null;
    String methodName = null;
    String orderId = null;
    try {
      Map<String, Object> map = aspectUtil.getRequestLog(joinPoint);

      orderId = mingshiServerUtil.getOrderId(null);
      // 请求信息插入到数据库中
      requestInfoIntoMysql(map, orderId);

      methodName = (String) map.get(Const.METHOD);
      // 获取用户名
      resObj = getUserName(map, request, instStart, orderId);
      if (null != resObj) {
        // 先暂时注释掉，等以后前端启用用户名登录后，再启用；2022-09-07 16:30:47
        return resObj;
      }
      userName = (String) map.get(Const.USER_NAME);

      // 调用具体的方法
      resObj = doExcute(joinPoint, map);

      map.put("desc", "用户已经登录，因为根据sessionId = " + request.getSession().getId() + " 在Redis中获取到了该用户的数据。");

      aspectUtil.outputLog(instStart, map, JsonUtil.obj2String(resObj), methodName);
    } catch (Exception e) {
      log.error("用户 = {} 调用接口 = {} 时，出现了异常", userName, methodName, e);
    } finally {
      // 将请求信息更新到表中
      update(userName, orderId, resObj);
      // 为了避免被前世的记忆干扰了今生的行为，最好使用完调用remove方法，将其删除；
      orderIdThreadLocal.remove();
    }
    return resObj;
  }

  /**
   * <B>方法名称：doExcute</B>
   * <B>概要说明：调用具体的业务方法</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年09月07日 15:09:10
   * @Param [joinPoint, request, map]
   **/
  private ServerResponse<String> doExcute(ProceedingJoinPoint joinPoint, Map<String, Object> map) {
    String methodName = (String) map.get(Const.METHOD);
    String userName = (String) map.get(Const.USER_NAME);
    ServerResponse<String> resObj = null;

    String oldPassword = (String) map.get("oldPassword");
    String newPassword = (String) map.get("newPassword");
    if (methodName.contains("getSysMenu") || methodName.contains("sysroles")) {
      /** 由于不能从前端传递用户的用户名过来，所以从这里把用户名传递过去（若是从前端传递用户名过来，不安全）*/
      resObj = aspectUtil.excute(joinPoint, userName);
    } else if (methodName.contains("changePassword")) {
      resObj = aspectUtil.excute(joinPoint, oldPassword, newPassword, userName);
    } else {
      resObj = aspectUtil.excute(joinPoint);
    }
    return resObj;
  }

  /**
   * <B>方法名称：getUserName</B>
   * <B>概要说明：从请求中获取用户名</B>
   *
   * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年09月07日 15:09:45
   * @Param [map, request, instStart, orderId]
   **/
  private ServerResponse<String> getUserName(Map<String, Object> map, HttpServletRequest request, Instant instStart, String orderId) {
    String userName = (String) map.get(Const.USER_NAME);
    String methodName = (String) map.get(Const.METHOD);
    HttpSession httpSession = request.getSession();
    String sessionId = httpSession.getId();
    /**对登录接口不进行登录校验*/
    if (!StringUtil.equals(methodName, "login")) {
      String str = String.valueOf(redisPoolUtil.get(sessionId));
      if (StringUtil.equals(str, "null")) {
        ServerResponse<String> resObj = new ServerResponse<>(AiitExceptionCode.USER_IS_NOT_LOGGED_IN);
        String errorStr = JsonUtil.obj2String(resObj);
        log.error("用户未登录 = {}，在Redis中没有获取到的用户的数据，返回给前端的数据 = {}", userName, errorStr);
        map.put("desc", "用户未登录，因为根据sessionId = " + sessionId + " 在Redis中没有获取到该用户的数据。");
        aspectUtil.outputLog(instStart, map, JsonUtil.obj2String(resObj), methodName);
        update(userName, orderId, resObj);
        return resObj;
      }
      SysOperator sysOperator = JsonUtil.string2Obj(str, SysOperator.class);
      userName = sysOperator.getUserName();
      map.put("userName", userName);
    }
    return null;
  }

  /**
   * <B>方法名称：requestInfoIntoMysql</B>
   * <B>概要说明：请求信息插入到数据库中</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年09月07日 14:09:54
   * @Param [map]
   **/
  private void requestInfoIntoMysql(Map<String, Object> map, String orderId) {
    String userName = null;
    try {
      String methodName = null;
      String ip = (String) map.get(Const.IP);
      methodName = (String) map.get(Const.METHOD);
      userName = (String) map.get(Const.USER_NAME);
      String params = (String) map.get(Const.PARAMS);
      String url = String.valueOf(map.get(Const.URL));
      orderIdThreadLocal.set(orderId);
      map.put(Const.ORDER_ID, orderId);

      /** 将请求信息插入表中*/
      OperationLog operationLog = new OperationLog();

      /* 获取操作说明 */
      getOperationDesc(params, operationLog, methodName);

      operationLog.setUserName(userName);
      operationLog.setGmtCreate(DateUtil.dateStr4(new Date()));
      operationLog.setLoginIp(ip);
      operationLog.setRequestParams(params);
      operationLog.setMethodName(methodName);
      operationLog.setRequestUrl(url);
      operationLog.setRequestParams(params);
      operationLog.setOrderId(orderId);
      Integer insertResult = operateLogService.insertSelective(operationLog);
      if (1 != insertResult) {
        log.error("将用户 = {} 的操作记录 = {} 保存到表中失败。", userName, JsonUtil.obj2String(operationLog));
      }
    } catch (Exception e) {
      log.error("# DataAccessAspect.requestInfoIntoMysql() # 将用户【{}】操作记录插入到表中出现了异常。", userName, e);
    }
  }

  /**
   * <B>方法名称：getOperationDesc</B>
   * <B>概要说明：获取操作说明</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年09月09日 14:09:37
   * @Param [params, operateLog, methodName]
   **/
  private void getOperationDesc(String params, OperationLog operationLog, String methodName) {
    ObjectNode jsonNodes = JsonUtil.string2Obj(params, ObjectNode.class);
    if (null != jsonNodes && methodName.equals(Const.UPDATE_MONITOR_TABLE)) {
      if (null != jsonNodes.get(Const.IS_DELETE_OPRETION)) {
        StringBuilder stringBuilder = new StringBuilder();
        String value = UpdateOperationRecord.getValue(methodName, jsonNodes.get(Const.IS_DELETE_OPRETION).asText());
        stringBuilder.append(value);
        if (null != jsonNodes.get(Const.TABLE_NAME)) {
          stringBuilder.append(Const.COLON);
          stringBuilder.append(jsonNodes.get(Const.TABLE_NAME).asText());
        }
        operationLog.setOperationDesc(stringBuilder.toString());
      }
    } else if (null != jsonNodes && methodName.equals(Const.UPDATE_MONITOR_TABLE_DESC)) {
      if (null != jsonNodes.get(Const.TABLE_DESC)) {
        StringBuilder stringBuilder = new StringBuilder();
        String value = UpdateOperationRecord.getValue(methodName, Const.TABLE_DESC);
        stringBuilder.append(value);
        String tableName = null;
        if (null != jsonNodes.get(Const.TABLE_NAME)) {
          tableName = jsonNodes.get(Const.TABLE_NAME).asText();
          stringBuilder.append(Const.COLON);
          stringBuilder.append(jsonNodes.get(Const.TABLE_DESC).asText());
        }
        operationLog.setOperationDesc(StringUtil.isBlank(tableName) == true ? stringBuilder.toString() : stringBuilder.toString().replace(Const.COMMA, tableName));
      }
    }else if(null != jsonNodes && methodName.equals(Const.UPDATE_SKYWALKING_AGENT)){
      if (null != jsonNodes.get(Const.AGENT_NAME)) {
        StringBuilder stringBuilder = new StringBuilder();
        String value = UpdateOperationRecord.getValue(methodName, Const.AGENT_NAME);
        stringBuilder.append(value);
        String agentName = null;
        if (null != jsonNodes.get(Const.AGENT_CODE)) {
          agentName = jsonNodes.get(Const.AGENT_CODE).asText();
          stringBuilder.append(Const.COLON);
          stringBuilder.append(jsonNodes.get(Const.AGENT_NAME).asText());
        }
        operationLog.setOperationDesc(StringUtil.isBlank(agentName) == true ? stringBuilder.toString() : stringBuilder.toString().replace(Const.COMMA, agentName));
      }
    }
  }

  /**
   * <B>方法名称：update</B>
   * <B>概要说明：更新用户操作</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年09月09日 14:09:19
   * @Param [userName, orderId, resObj]
   **/
  private void update(String userName, String orderId, ServerResponse<String> resObj) {
    try {
      // 将请求信息更新到表中
      OperationLog operationLog = operateLogService.selectByOrderId(orderId);
      if (null != operationLog) {
        operationLog.setResponseParams(JsonUtil.obj2String(resObj));
        operationLog.setUserName(userName);
        operationLog.setGmtModified(DateUtil.dateStr4(new Date()));
        Integer updateResult = operateLogService.updateByPrimaryKeySelective(operationLog);
        if (1 != updateResult) {
          log.error("将用户 = {} 的操作记录 = {}更新到表中失败。", userName, JsonUtil.obj2String(operationLog));
        }
      }
    } catch (Exception e) {
      log.error("将用户 = {} 的操作信息更新到表中出现异常。", userName);
    }
  }
}

