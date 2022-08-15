package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.MsThirdPartyTableFieldsDo;
import com.mingshi.skyflying.common.response.ServerResponse;

import java.util.Map;

/**
 * <B>接口名称: MsThirdPartyTableFieldsService</B>
 * <B>概要说明：获取一个表中所有的字段信息</B>
 * @Author zm
 * Date 2022/6/20 14:32
 *
 * @Version 1.0
 **/
public interface MsThirdPartyTableFieldsService extends ParentService<MsThirdPartyTableFieldsDo, Long> {
  ServerResponse<String> getAllTableFieldsName(Map<String, String> map);

  ServerResponse<String> getSpecificDbTableNameFields(String dbName, String tableName, Integer pageNo, Integer pageSize);

  ServerResponse<String> updateSpecificDbTableNameFields(Integer id, String field, String fieldName, String fieldNote);
}
