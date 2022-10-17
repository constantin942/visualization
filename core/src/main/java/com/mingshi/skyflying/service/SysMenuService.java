package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.SysMenu;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.service.ParentService;

/**
 *
 *@类描述：菜单服务
 *@author 49090 2017年12月26日上午11:33:16
 *本内容仅限于北大信研院安全工程与云计算实验室内部传阅，禁止外泄以及用于其他的商业目的
 */
public interface SysMenuService extends ParentService<SysMenu, Long> {

    ServerResponse<String> getSysMenu(String userName);

}
