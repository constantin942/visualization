package com.aiit.web.controller;

import com.aiit.skyflying.common.constant.Const;
import com.aiit.skyflying.common.domain.SysOperator;
import com.aiit.skyflying.common.domain.UserLoginLog;
import com.aiit.skyflying.common.exception.AiitExceptionCode;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.utils.DateTimeUtil;
import com.aiit.skyflying.common.utils.JsonUtil;
import com.aiit.skyflying.common.utils.MingshiServerUtil;
import com.aiit.skyflying.common.utils.RedisPoolUtil;
import com.aiit.skyflying.service.AiitSysUsersService;
import com.aiit.skyflying.service.UserLoginLogService;
import jodd.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.time.Instant;
import java.util.Date;

/**
 * @Author zhaoming
 * @Description 不需要登录校验的接口写这里面
 * @Date 15:28 2020/2/2
 * @Param
 * @return
 **/
@RestController
@Slf4j
@RequestMapping("/api/user")
public class UserController {

    @Resource
    private AiitSysUsersService aiitSysUsersService;
    @Resource
    private RedisPoolUtil redisPoolUtil;
    @Resource
    private UserLoginLogService userLoginLogService;

    /**
     * @return com.zhejiang.mobile.common.response.ServerResponse
     * @Author zhaoming
     * @Description 用户登录
     * @Date 下午3:40 2021/6/8
     * @Param [request, userName, password]
     **/
    @PostMapping(value = "/login")
    public ServerResponse<String> login(HttpServletRequest request, @RequestParam(value = "userName", required = true) String userName, @RequestParam(value = "password", required = true) String password) {
        Instant instStart = Instant.now();
        UserLoginLog userLoginLog = new UserLoginLog();
        userLoginLog.setUserName(userName);
        userLoginLog.setDescription(Const.LOGIN_DESC);
        userLoginLog.setLoginIp(MingshiServerUtil.getIpAddress(request));
        userLoginLog.setGmtCreate(new Date());
        userLoginLog.setGmtModified(new Date());
        ServerResponse<String> response = aiitSysUsersService.login(userName, password);
        if (AiitExceptionCode.SUCCESS.getCode().equals(response.getCode()) && !StringUtil.equals(null, String.valueOf(response.getData()))) {
            HttpSession oldSession = request.getSession(false);
            if (oldSession != null) {
                oldSession.invalidate();
            }
            HttpSession httpSession = request.getSession(true);
            String sessionId = httpSession.getId();
            log.info("用户=【{}】登录  sessionID={}", userName, sessionId);
            boolean flag = redisPoolUtil.set(sessionId, response.getData(), Const.REDIS_SESSION_EXTIME);
            log.info("用户 phone={} 登录成功，将用户的信息放入Redis中的结果={}", userName, flag);
            userLoginLog.setSessionId(sessionId);
            userLoginLog.setResult(Const.SUCCESS);
        } else {
            log.info("用户 phone={} 登录失败。", userName);
            userLoginLog.setResult(Const.FAILED);
        }
        Integer insertResult = userLoginLogService.insertSelective(userLoginLog);
        if (!Const.NUMBER_ONE.equals(insertResult)) {
            log.error("# UserController.login() # 将用户登录结果【{}】插入到数据库中失败。", JsonUtil.obj2String(userLoginLog));
        }
        log.info("用户 phone={}登录结束，返回给前端的信息={}，接口执行时间={} 毫秒", userName, JsonUtil.obj2String(response), DateTimeUtil.getTimeMillis(instStart));
        return response;
    }

    /**
     * @return com.serverless.common.response.ServerResponse<com.serverless.common.domain.AiitUsers>
     * @Author zhaoming
     * @Description 判断用户是否已登录
     * @Date 15:19 2020/2/3
     * @Param [userName, password, request, httpServletResponse]
     **/
    @PostMapping(value = "/isLogin")
    public ServerResponse<String> isLogin(HttpServletRequest request, HttpServletResponse httpServletResponse) {
        ServerResponse<String> serverResponse = new ServerResponse<>(AiitExceptionCode.SUCCESS);
        HttpSession httpSession = request.getSession();
        String sessionId = httpSession.getId();
        log.info("判断用户是否已登录  sessionID={}", sessionId);
        String str = String.valueOf(redisPoolUtil.get(sessionId));
        if (StringUtil.equals(str, Const.IS_NULL)) {
            log.info("用户登录已过期");
            return new ServerResponse<>(AiitExceptionCode.USER_IS_NOT_LOGGED_IN);
        }
        SysOperator aiitUsers = JsonUtil.string2Obj(str, SysOperator.class);
        String userName = aiitUsers.getUserName();
        serverResponse.setData(userName);
        log.info("用户登录未过期");
        return serverResponse;
    }

    /**
     * @return com.serverless.common.response.ServerResponse<java.lang.String>
     * @Author zhaoming
     * @Description 退出登录
     * @Date 18:43 2020/2/3
     * @Param [request, httpServletResponse]
     **/
    @PostMapping(value = "/loginOut")
    public ServerResponse<String> loginOut(HttpServletRequest request) {
        ServerResponse<String> serverResponse = new ServerResponse<>(AiitExceptionCode.SUCCESS);
        HttpSession httpSession = request.getSession();
        String sessionId = httpSession.getId();
        log.info("用户退出登录  sessionID={}", sessionId);
        updateUserLoginLog(sessionId);
        boolean str = redisPoolUtil.del(sessionId);
        if (false == str) {
            log.info("用户退出登录失败，因为根据sessionID将用户的信息从Redis中删除失败，sessionID={}", sessionId);
            return new ServerResponse<>(AiitExceptionCode.USER_IS_NOT_LOGGED_IN);
        }
        log.info("用户退出登录成功，sessionID={}", sessionId);

        //注销后更换session
        httpSession.invalidate();
        request.getSession();
        return serverResponse;
    }

    private void updateUserLoginLog(String sessionId) {
        UserLoginLog aiitUserLoginLog = userLoginLogService.selectBySeesionId(sessionId);
        if (null != aiitUserLoginLog) {
            aiitUserLoginLog.setGmtModified(new Date());
            aiitUserLoginLog.setDescription(Const.LOGIN_OUT_DESC);
            Integer updateResult = userLoginLogService.updateByPrimaryKeySelective(aiitUserLoginLog);
            if (1 != updateResult) {
                log.error("用户退出登录，根据sessionId={} 在用户登录表中更新登录记录失败。", sessionId);
            }
        } else {
            log.error("用户退出登录，根据sessionId={}在用户登录表中没找到登录记录。", sessionId);
        }
    }
}
