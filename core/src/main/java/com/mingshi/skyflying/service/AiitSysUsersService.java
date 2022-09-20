package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.SysOperator;
import com.mingshi.skyflying.common.response.ServerResponse;

/**
 * @author 49090 2017年12月26日上午11:33:16
 * 本内容仅限于北大信研院安全工程与云计算实验室内部传阅，禁止外泄以及用于其他的商业目的
 * @类描述：菜单服务
 */
public interface AiitSysUsersService extends ParentService<SysOperator, Long> {

  int insertSelective(SysOperator sysOperator);

  SysOperator selectByPrimaryKey(Integer id);

  int updateByPrimaryKeySelective(SysOperator sysOperator);

  ServerResponse<String> addSysUser(String userName, String password, String phone, Integer roleId, String creator) throws Exception;

  ServerResponse<String> changePassword(String userName, String oldPassword, String newPassword);

  ServerResponse<SysOperator> login(String userName, String password);

}
