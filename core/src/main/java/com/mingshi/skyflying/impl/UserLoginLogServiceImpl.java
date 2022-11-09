package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.UserLoginLog;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.dao.UserLoginLogMapper;
import com.mingshi.skyflying.service.UserLoginLogService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 *@类描述：
 *@author 49090 2017年12月26日上午11:34:17
 *
 */
@Slf4j
@Service("aiitUserLoginLogService")
public class UserLoginLogServiceImpl extends BaseParentServiceImpl<UserLoginLog, Long> implements UserLoginLogService {

	@Resource
  UserLoginLogMapper userLoginLogMapper;

	@Override
	public int insertSelective(UserLoginLog userLoginLog) {
		return userLoginLogMapper.insertSelective(userLoginLog);
	}

	@Override
	public UserLoginLog selectByPrimaryKey(Long id) {
		return userLoginLogMapper.selectByPrimaryKey(id);
	}

    @Override
    public ServerResponse<String> selectAll(Integer pageNo, Integer pageSize) {
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        Map<String, Integer> queryMap = new HashMap<>();
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);
        List<UserLoginLog> userLoginLogs = userLoginLogMapper.selectAll(queryMap);
        Integer count = userLoginLogMapper.selectAllCount(queryMap);
        ObjectNode context = JsonUtil.createJsonObject();
        if (null != userLoginLogs && !userLoginLogs.isEmpty()) {
            context.put("rows", JsonUtil.object2String(userLoginLogs));
        }
        context.put("total", count);
        bySuccess.setData(JsonUtil.object2String(context));

        log.info("# OperationLogServiceImpl.selectAll() # 根据查询条件【{}】，获取用户登录日志成功。", queryMap);
        return bySuccess;
    }

    @Override
	public UserLoginLog selectBySeesionId(String sessionId) {
		return userLoginLogMapper.selectBySeesionId(sessionId);
	}

	@Override
	public int updateByPrimaryKeySelective(UserLoginLog userLoginLog) {
		return userLoginLogMapper.updateByPrimaryKeySelective(userLoginLog);
	}

}

