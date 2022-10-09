package com.mingshi.skyflying.anomaly_detection.service.impl;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.UserPortraitRulesDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.RedisPoolUtil;
import com.mingshi.skyflying.anomaly_detection.dao.UserPortraitRulesMapper;
import com.mingshi.skyflying.anomaly_detection.service.UserPortraitRulesService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：UserPortraitRulesServiceImpl</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022年06月23日 15:06:59
 * @Param
 * @return
 **/
@Slf4j
@Service("userPortraitRulesService")
public class UserPortraitRulesServiceImpl implements UserPortraitRulesService {
    @Resource
    private UserPortraitRulesMapper userPortraitRulesMapper;
    private String PREFIX = "anomaly_detection:enableRule:";
    @Resource
    RedisPoolUtil redisPoolUtil;

    private static final String TIME_SUF = "time";

    private static final String TABLE_SUF = "table";

    private final Integer EXPIRE = 1000;

    @Override
    public ServerResponse<String> getAllUserPortraitRules(Integer pageNo, Integer pageSize) {
        ServerResponse<String> bySuccess = null;
        try {
            Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
            if (null == pageNo) {
                pageNo = 1;
            }
            if (null == pageSize) {
                pageSize = 10;
            }
            queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
            queryMap.put(Const.PAGE_SIZE, pageSize);

            List<UserPortraitRulesDo> userPortraitRulesDoList = null;
            userPortraitRulesDoList = userPortraitRulesMapper.selectAllRules(queryMap);
            log.info("执行 # UserPortraitRulesServiceImpl.getAllUserPortraitRules() # 获取所有的规则信息。根据查询条件【{}】获取到的规则信息是【{}】。", JsonUtil.obj2String(queryMap), JsonUtil.obj2String(userPortraitRulesDoList));

            Integer count = userPortraitRulesMapper.selectAllRulesCount(queryMap);
            Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
            bySuccess = ServerResponse.createBySuccess();
            context.put("rows", JsonUtil.obj2String(userPortraitRulesDoList));
            context.put("total", count);
            bySuccess.setData(JsonUtil.obj2String(context));
        } catch (Exception e) {
            log.info(" # UserPortraitRulesServiceImpl.getAllUserPortraitRules() # 获取所有的规则信息时，出现了异常。", e);
        }
        log.info("执行完毕 # UserPortraitRulesServiceImpl.getAllUserPortraitRules() # 获取所有的规则信息。");
        return bySuccess;
    }

    @Override
    public ServerResponse<String> updateUserPortraitRule(Integer ruleId, Integer isDelete) {
        log.info("开始执行 # UserPortraitRulesServiceImpl.updateUserPortraitRule() # 更新用户在什么时间访问过的系统次数画像规则启用状态。 ");
        if (!isDelete.equals(Const.IS_DELETE_ZERO) && !isDelete.equals(Const.IS_DELETE_ONE)) {
            return ServerResponse.createByErrorMessage("参数非法：是否启用的参数isDelete应该是0或者1.", "");
        }

        // 先根据规则id在数据库中找到这条规则；2022-06-16 14:44:21
        UserPortraitRulesDo userPortraitRulesDo = userPortraitRulesMapper.selectByPrimaryKey(ruleId);
        if (null == userPortraitRulesDo) {
            return ServerResponse.createByErrorMessage("参数非法：规则id在数据库中不存在.", "");
        }

        Integer isDelete1 = userPortraitRulesDo.getIsDelete();
        // 如果数据库中该条规则的状态已经是当前要更新的状态，那么就结束当前请求；2022-06-16 17:36:58
        if (isDelete1.equals(isDelete)) {
            return ServerResponse.createBySuccess();
        }
        // 设置规则启用/禁用的状态；2022-06-16 15:20:09
        cacheRule(ruleId, isDelete);
        userPortraitRulesDo.setIsDelete(isDelete);
        // 更新本地内存；2022-06-16 14:49:29
        ServerResponse<String> response = doUpdateUserPortraitRuleRule(userPortraitRulesDo, isDelete);
        log.info("执行完毕 # UserPortraitRulesServiceImpl.updateUserPortraitRule() # 更新用户访问过的表的画像规则启用状态。 ");
        return response;
    }

    /**
     *  存入Redis
     */
    @Override
    public Boolean cacheRule(Integer ruleId, Integer isDelete) {
        Boolean enable = isDelete != 1;
        if(ruleId == 1) {
            redisPoolUtil.set(PREFIX + TIME_SUF, enable, EXPIRE);
        }
        if (ruleId == 2) {
            redisPoolUtil.set(PREFIX + TABLE_SUF, enable, EXPIRE);
        }
        return enable;
    }

    @Override
    public void updateRule(Integer ruleId, Integer isDelete) {
        UserPortraitRulesDo userPortraitRulesDo = userPortraitRulesMapper.selectByPrimaryKey(ruleId);
        userPortraitRulesMapper.updateByPrimaryKeySelective(userPortraitRulesDo);
        cacheRule(ruleId, isDelete);
    }

    private ServerResponse<String> doUpdateUserPortraitRuleRule(UserPortraitRulesDo userPortraitRulesDo, Integer isDelete) {
        if (isDelete.equals(Const.IS_DELETE_ONE)) {
            // 禁用这条规则；2022-06-16 14:55:51
            return noEnableByUserPortraitRule(userPortraitRulesDo);
        }
        if (isDelete.equals(Const.IS_DELETE_ZERO)) {
            // 启用这条规则；2022-06-16 14:55:51
            return enableByUserPortraitRule(userPortraitRulesDo);
        }
        return null;
    }

    /**
     * <B>方法名称：noEnableByUserPortraitRule</B>
     * <B>概要说明：禁用规则：规则先在数据库中禁用，然后在本地内存中删除</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年06月16日 17:06:20
     * @Param [userPortraitByVisitedTimeDo]
     **/
    private ServerResponse<String> noEnableByUserPortraitRule(UserPortraitRulesDo userPortraitRulesDo) {
        int updateResult = userPortraitRulesMapper.updateByPrimaryKeySelective(userPortraitRulesDo);
        if (1 != updateResult) {
            log.error(" # UserPortraitByVisitedVisitedTimeServiceImpl.noEnableByUserPortraitByVisitedTime() # 把禁用这条规则的状态更新到数据库中失败。");
            return ServerResponse.createByErrorMessage("更新数据库操作失败", "");
        }
        return ServerResponse.createBySuccess();
    }

    /**
     * <B>方法名称：enableByUserPortraitByVisitedTime</B>
     * <B>概要说明：将用户在什么时间访问多少次系统这个规则先在数据库中启用，然后添加到本地内存中</B>
     *
     * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
     * @Author zm
     * @Date 2022年06月16日 17:06:20
     * @Param [userPortraitByVisitedTimeDo]
     **/
    private ServerResponse<String> enableByUserPortraitRule(UserPortraitRulesDo userPortraitRulesDo) {
        int updateResult = userPortraitRulesMapper.updateByPrimaryKeySelective(userPortraitRulesDo);
        if (1 != updateResult) {
            log.error(" # UserPortraitByVisitedVisitedTimeServiceImpl.enableByUserPortraitByVisitedTime() # 把启用这条规则的状态更新到数据库中失败。");
            return ServerResponse.createByErrorMessage("更新数据库操作失败", "");
        }
        return ServerResponse.createBySuccess();
    }

}
