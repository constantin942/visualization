package com.mingshi.skyflying.service;

import com.mingshi.skyflying.domain.MsThirdPartyTableListDo;
import com.mingshi.skyflying.response.ServerResponse;

/**
 * <B>接口名称: MsThirdPartyTableService</B>
 * <B>概要说明：根据指定的数据库名称，获取这个数据库中所有的表</B>
 * Author zm
 * Date 2022/6/20 14:32
 *
 * @Version 1.0
 **/
public interface MsThirdPartyTableListService extends ParentService<MsThirdPartyTableListDo, Long> {
  ServerResponse<String> getAllTableNames(String dbName);
}
