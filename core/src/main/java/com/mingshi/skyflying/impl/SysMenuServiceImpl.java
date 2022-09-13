package com.mingshi.skyflying.impl;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mingshi.skyflying.common.domain.SysMenu;
import com.mingshi.skyflying.common.domain.SysMenuRole;
import com.mingshi.skyflying.common.domain.SysOperator;
import com.mingshi.skyflying.common.domain.SysOperatorRole;
import com.mingshi.skyflying.common.exception.AiitExceptionCode;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.dao.SysMenuDao;
import com.mingshi.skyflying.dao.SysMenuRoleDao;
import com.mingshi.skyflying.dao.SysOperatorDao;
import com.mingshi.skyflying.dao.SysOperatorRoleDao;
import com.mingshi.skyflying.service.SysMenuService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * @author 49090 2017年12月26日上午11:34:48
 * 本内容仅限于北大信研院安全工程与云计算实验室内部传阅，禁止外泄以及用于其他的商业目的
 * @类描述：
 */
@Slf4j
@Service("sysMenuService")
public class SysMenuServiceImpl extends ParentServiceImpl<SysMenu, Long> implements SysMenuService {

  @Resource
  private SysOperatorDao sysOperatorDao;
  @Resource
  private SysOperatorRoleDao sysOperatorRoleDao;
  @Resource
  private SysMenuRoleDao sysMenuRoleDao;
  @Resource
  private SysMenuDao sysMenuDao;

  @Override
  public ServerResponse<String> getSysMenu(String userName) {
    // 根据userName获取操作用户的id；
    SysOperator sysOperator = sysOperatorDao.selectByUserName(userName);
    if (null == sysOperator) {
      log.error("# SysMenuServiceImpl.getSysMenu() # 根据用户名=【{}】在系统用户表中没有找到对应的信息。", userName);
      return ServerResponse.createByErrorMessage(AiitExceptionCode.USERNAME_DOES_NOT_EXIST.getMsgCn(), "");
    }
    // 根据userName获取对应的角色id；2021-06-09 09:51:03
    SysOperatorRole sysOperatorRole = sysOperatorRoleDao.selectBySysOperatorId(sysOperator.getId());
    if (null == sysOperatorRole) {
      log.error("# SysMenuServiceImpl.getSysMenu() # 根据用户名=【{}】用户id=【{}】在系统用户角色表中没有找到对应的角色信息。", userName, sysOperator.getId());
      return ServerResponse.createByErrorMessage(AiitExceptionCode.USERNAME_ROLE_NOT_EXIST.getMsgCn(), "");
    }
    // 根据用户角色id获取对应的菜单id；2021-06-09 10:02:53
    List<SysMenuRole> sysMenuRoleList = sysMenuRoleDao.selectByRoleId(sysOperatorRole.getRoleId());
    if (null == sysMenuRoleList || 0 == sysMenuRoleList.size()) {
      log.error("# SysMenuServiceImpl.getSysMenu() # 根据用户名=【{}】用户角色id=【{}】在系统菜单角色表中没有找到对应的角色对应的菜单信息。", userName, sysOperatorRole.getRoleId());
      return ServerResponse.createByErrorMessage(AiitExceptionCode.USERNAME_ROLE_MENU_NOT_EXIST.getMsgCn(), "");
    }
    Set<SysMenu> sysMenuSet = new LinkedHashSet<>();

    // 根据角色id获取只读标志；2022-09-09 09:54:57
    Integer readOnly = sysMenuRoleDao.selectReadOnly(sysOperatorRole.getRoleId());

    // 根据菜单id获取对应的菜单；2021-06-09 10:07:09
    for (SysMenuRole sysMenuRole : sysMenuRoleList) {
      SysMenu sysMenu = sysMenuDao.selectByPrimaryKey(sysMenuRole.getMenuId());
      if (null != sysMenu) {
        sysMenuSet.add(sysMenu);
      }
    }
    ObjectNode jsonObject = JsonUtil.createJsonObject();
    jsonObject.put("readOnly", readOnly);
    jsonObject.put("menu", JsonUtil.obj2String(sysMenuSet));

    ServerResponse<String> serverResponse = new ServerResponse(AiitExceptionCode.SUCCESS);
    serverResponse.setData(jsonObject.toString());
    return serverResponse;
  }
}
