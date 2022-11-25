package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.dao.MsUserFromMapper;
import com.mingshi.skyflying.common.domain.MsUserFrom;
import com.mingshi.skyflying.common.domain.SysOperator;
import com.mingshi.skyflying.common.domain.UserLoginLog;
import com.mingshi.skyflying.common.domain.UserLoginStatistics;
import com.mingshi.skyflying.common.exception.AiitExceptionCode;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.DateUtil;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.UserUtil;
import com.mingshi.skyflying.dao.SysOperatorDao;
import com.mingshi.skyflying.dao.SysOperatorRoleDao;
import com.mingshi.skyflying.service.AiitSysUsersService;
import com.mingshi.skyflying.service.UserFromService;
import com.mingshi.skyflying.service.UserLoginStatisticsService;
import jodd.util.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class UsersFromServiceImpl extends BaseParentServiceImpl<MsUserFrom, Long> implements UserFromService {

  @Resource
  private MsUserFromMapper msUserFromMapper;

    @Override
    public ServerResponse<String> selectAll(Integer pageNo, Integer pageSize) {
        ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
        Map<String, Integer> queryMap = new HashMap<>(Const.INITAL_SIZE);
        if (null == pageNo) {
            pageNo = 1;
        }
        if (null == pageSize) {
            pageSize = 10;
        }
        queryMap.put(Const.PAGE_NO, (pageNo - 1) * pageSize);
        queryMap.put(Const.PAGE_SIZE, pageSize);
        List<MsUserFrom> userLoginLogs = msUserFromMapper.selectAll(queryMap);
        Integer count = msUserFromMapper.selectAllCount();
        ObjectNode context = JsonUtil.createJsonObject();
        if (null != userLoginLogs && !userLoginLogs.isEmpty()) {
            context.put("rows", JsonUtil.object2String(userLoginLogs));
        }
        context.put("total", count);
        bySuccess.setData(JsonUtil.object2String(context));

        log.info("# UsersFromServiceImpl.selectAll() # 根据查询条件【{}】，获取用户来源信息成功。", queryMap);
        return bySuccess;
    }

    /**
     * <B>方法名称：updateUserFrom</B>
     * <B>概要说明：禁启用统计用户来源</B>
     *
     * @Author zm
     * @Date 2022-11-25 15:24:15
     * @Param [ruleId, isDelete]
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     **/
    @Override
    public ServerResponse<String> updateUserFrom(Integer ruleId, Integer isDelete) {
        if(null ==  ruleId){
            return ServerResponse.createByErrorMessage("参数 ruleId 不能为空。","");
        }
        if(0 > ruleId){
            return ServerResponse.createByErrorMessage("参数 ruleId 不能小于0。","");
        }

        if(!isDelete.equals(Const.NUMBER_ZERO) && !isDelete.equals(Const.NUMBER_ONE)){
            return ServerResponse.createByErrorMessage("参数 isDelete 只能为0（禁用）或者1（启用）。","");
        }

        MsUserFrom msUserFrom = msUserFromMapper.selectByPrimaryKey(ruleId);
        if(null == msUserFrom){
            return ServerResponse.createByErrorMessage("根据参数 ruleId ，在数据库中没有获取到数据。","");
        }
        msUserFrom.setIsDelete(isDelete);

        int updateResult = msUserFromMapper.updateByPrimaryKeySelective(msUserFrom);
        if(!Const.NUMBER_ONE.equals(updateResult)){
            return ServerResponse.createByErrorMessage("更新失败。","");
        }
        return ServerResponse.createBySuccess();
    }

    /**
     * <B>方法名称：addUserFrom</B>
     * <B>概要说明：增加用户来源</B>
     *
     * @Author zm
     * @Date 2022-11-25 15:26:18
     * @Param [userFromPath, userFromDesc]
     * @return com.mingshi.skyflying.common.response.ServerResponse<java.lang.String>
     **/
    @Override
    public ServerResponse<String> addUserFrom(String userFromPath, String userFromDesc) {
        if(StringUtils.isBlank(userFromPath)){
            return ServerResponse.createByErrorMessage("参数 userFromPath （用户访问系统的接口路径）不能为空。","");
        }
        if(StringUtils.isBlank(userFromDesc)){
            return ServerResponse.createByErrorMessage("参数 userFromDesc （用户访问系统的接口路径的中文说明）不能为空。","");
        }
        MsUserFrom msUserFrom = msUserFromMapper.selectByUserFromPath(userFromPath);
        if(null != msUserFrom){
            return ServerResponse.createByErrorMessage("用户来源配置信息已存在，请勿重复插入。","");
        }
        msUserFrom = new MsUserFrom();
        msUserFrom.setFromName(userFromDesc);
        msUserFrom.setFromPath(userFromPath);
        msUserFrom.setGmtCreate(new Date());
        msUserFrom.setGmtModified(new Date());
        int insertResult = msUserFromMapper.insertSelective(msUserFrom);
        if(!Const.NUMBER_ONE.equals(insertResult)){
            return ServerResponse.createByErrorMessage("新增用户来源配置信息失败。","");
        }

        return ServerResponse.createBySuccess();
    }
}
