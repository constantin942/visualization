package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.dao.MsThirdPartyTableFieldsMapper;
import com.mingshi.skyflying.dao.MsThirdPartyTableListMapper;
import com.mingshi.skyflying.domain.MsThirdPartyTableFieldsDo;
import com.mingshi.skyflying.domain.MsThirdPartyTableListDo;
import com.mingshi.skyflying.response.ServerResponse;
import com.mingshi.skyflying.service.MsThirdPartyTableFieldsService;
import com.mingshi.skyflying.utils.JsonUtil;
import com.mingshi.skyflying.utils.StringUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;

/**
 * <B>方法名称：MsThirdPartTableFieldsServiceImpl</B>
 * <B>概要说明：获取数据库中指定的表有哪些字段</B>
 *
 * @Author zm
 * @Date 2022年06月20日 14:06:48
 * @Param
 * @return
 **/
@Slf4j
@Service("msThirdPartyTableFieldsService")
public class MsThirdPartTableFieldsServiceImpl implements MsThirdPartyTableFieldsService {

  @Resource
  private MsThirdPartyTableListMapper msThirdPartyTableListMapper;
  @Resource
  private MsThirdPartyTableFieldsMapper msThirdPartyTableFieldsMapper;

  @Override
  public ServerResponse<String> getAllTableFieldsName(Map<String, String> map) {

    Map<String/* 数据库名称 */, Map<String/* 表名称 */, Integer/* 记录id */>> dbNameTableNameIdMap = new HashMap<>();
    log.info("开始执行 # MsThirdPartTableListServiceImpl.getAllTableNames() # 根据数据库名称获取这个数据库中所有的表。");

    LinkedList<MsThirdPartyTableFieldsDo> msThirdPartyTableFieldsDos = new LinkedList<>();
    if (null != map && 0 < map.size()) {
      Iterator<String> iterator = map.keySet().iterator();
      while (iterator.hasNext()) {
        String dbName = iterator.next();

        List<MsThirdPartyTableListDo> msThirdPartyTableListDoList = msThirdPartyTableListMapper.selectAllTablesByDbName(dbName);
        if (null != msThirdPartyTableListDoList && 0 < msThirdPartyTableListDoList.size()) {
          putAllTablesIntoLocalMemory(dbNameTableNameIdMap, msThirdPartyTableListDoList);
        }
        String tables = map.get(dbName);
        List<String> list = JsonUtil.string2Obj(tables, List.class, String.class);
        if (null != list && 0 < list.size()) {
          for (int i = 0; i < list.size(); i++) {
            String tableName = list.get(i);
            List<Map<String, Object>> fieldList = msThirdPartyTableFieldsMapper.selectAllFields(tableName);
            if (null != fieldList && 0 < fieldList.size()) {
              Map<String, Integer> tableNameIdMap = dbNameTableNameIdMap.get(dbName);
              Integer recordId = null;
              if (null != tableNameIdMap) {
                recordId = tableNameIdMap.get(tableName);
              }
              for (Map<String, Object> stringObjectMap : fieldList) {
                String fColumn = String.valueOf(stringObjectMap.get("fColumn"));
                String fRemark = String.valueOf(stringObjectMap.get("fRemark"));
                String fDatatype = String.valueOf(stringObjectMap.get("fDatatype"));
                MsThirdPartyTableFieldsDo msThirdPartyTableFieldsDo = new MsThirdPartyTableFieldsDo();
                msThirdPartyTableFieldsDo.setField(fColumn);
                msThirdPartyTableFieldsDo.setFieldNote(fRemark);
                msThirdPartyTableFieldsDo.setFeldType(fDatatype);
                msThirdPartyTableFieldsDo.setThirdPartyNameId(recordId);
                msThirdPartyTableFieldsDos.add(msThirdPartyTableFieldsDo);
              }
            }
          }
        }
      }
    }

    if (0 < msThirdPartyTableFieldsDos.size()) {
      // 将每个表中域的信息插入到表中；2022-06-21 09:14:32
      batchInsert(msThirdPartyTableFieldsDos);
    }

    Map<String, List<String>> dbNameTableMap = new HashMap<>();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    if (0 < dbNameTableMap.size()) {
      log.info("开始执行 # MsThirdPartTableListServiceImpl.getAllTableNames() # 根据数据库名称获取这个数据库中所有的表。获取到的数据库名称和对应的数据库表有【{}】。", JsonUtil.obj2String(dbNameTableMap));
    }
    log.info("执行结束 # MsThirdPartTableListServiceImpl.getAllTableNames() # 根据数据库名称获取这个数据库中所有的表。");
    return bySuccess;
  }

  /**
   * <B>方法名称：getSpecificDbTableNameFields</B>
   * <B>概要说明：根据指定的数据库名和表名，获取该表所有的字段。</B>
   *
   * @return com.mingshi.skyflying.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年06月21日 09:06:44
   * @Param [dbName, tableName]
   **/
  @Override
  public ServerResponse<String> getSpecificDbTableNameFields(String dbName, String tableName, Integer pageNo, Integer pageSize) {
    log.info("开始执行 # MsThirdPartTableFieldsServiceImpl.getSpecificDbTableNameFields() # 根据指定的数据库名和表名，获取该表所有的字段。");
    Map<String, Object> queryMap = new HashMap<>();
    if (StringUtil.isNotBlank(dbName)) {
      queryMap.put("dbName", dbName);
    }
    if (StringUtil.isNotBlank(tableName)) {
      queryMap.put("tableName", tableName);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);

    List<MsThirdPartyTableFieldsDo> msThirdPartyTableFieldsList = new LinkedList<>();
    List<Map<String, Object>> msThirdPartyTableListDoIdList = msThirdPartyTableListMapper.selectByDbNameAndTableName(queryMap);
    if (null != msThirdPartyTableListDoIdList && 0 < msThirdPartyTableListDoIdList.size()) {
      msThirdPartyTableFieldsList = msThirdPartyTableFieldsMapper.selectByThirdPartyNameId(msThirdPartyTableListDoIdList);
      log.info("开始执行 # MsThirdPartTableFieldsServiceImpl.getSpecificDbTableNameFields() # 根据指定的数据库名和表名，获取该表所有的字段。根据查询条件 = 【{}】，查询到了【{}】条数据。", JsonUtil.obj2String(queryMap), msThirdPartyTableFieldsList.size());
    }

    Map<String, Object> context = new HashMap<>();
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    context.put("rows", JsonUtil.obj2String(msThirdPartyTableFieldsList));
    context.put("total", msThirdPartyTableFieldsList.size());
    bySuccess.setData(JsonUtil.obj2String(context));
    log.info("执行完毕 # MsThirdPartTableFieldsServiceImpl.getSpecificDbTableNameFields() # 获取来自DMS的数据库审计日志信息。");
    return bySuccess;
  }

  @Override
  public ServerResponse<String> updateSpecificDbTableNameFields(Integer id, String field, String fieldName, String fieldNote) {
    log.info("开始执行 # MsThirdPartTableFieldsServiceImpl.updateSpecificDbTableNameFields() # 更新表id = 【{}】中的记录信息。", id);
    if (null == id) {
      return ServerResponse.createByErrorMessage("记录id不能为空。", "");
    }
    MsThirdPartyTableFieldsDo msThirdPartyTableFieldsDo = new MsThirdPartyTableFieldsDo();
    msThirdPartyTableFieldsDo.setId(id);
    if (StringUtil.isNotBlank(field)) {
      msThirdPartyTableFieldsDo.setField(field);
    }
    if (StringUtil.isNotBlank(fieldName)) {
      msThirdPartyTableFieldsDo.setFieldName(fieldName);
    }
    if (StringUtil.isNotBlank(fieldNote)) {
      msThirdPartyTableFieldsDo.setFieldNote(fieldNote);
    }
    msThirdPartyTableFieldsMapper.updateByPrimaryKeySelective(msThirdPartyTableFieldsDo);
    log.info("执行完毕 # MsThirdPartTableFieldsServiceImpl.updateSpecificDbTableNameFields() # 更新表id = 【{}】中的记录信息。", id);
    return ServerResponse.createBySuccess();
  }

  // 从数据库中根据数据库名称获取对应的表，并放入到本地内存中；2022-06-21 09:35:25
  private void putAllTablesIntoLocalMemory(Map<String/* 数据库名称 */, Map<String/* 表名称 */, Integer/* 记录id */>> dbNameTableNameIdMap, List<MsThirdPartyTableListDo> msThirdPartyTableListDoList) {
    for (MsThirdPartyTableListDo msThirdPartyTableListDo : msThirdPartyTableListDoList) {
      String thirdPartyDbName = msThirdPartyTableListDo.getThirdPartyDbName();
      if (StringUtil.isNotBlank(thirdPartyDbName)) {
        Map<String, Integer> tableNameIdMap = dbNameTableNameIdMap.get(thirdPartyDbName);
        if (null == tableNameIdMap) {
          tableNameIdMap = new HashMap<>();
          dbNameTableNameIdMap.put(thirdPartyDbName, tableNameIdMap);
        }
        Integer id = msThirdPartyTableListDo.getId();
        String thirdPartyTableName = msThirdPartyTableListDo.getThirdPartyTableName();
        if (null != id && StringUtil.isNotBlank(thirdPartyTableName)) {
          tableNameIdMap.put(thirdPartyTableName, id);
        }
      }
    }
  }

  /**
   * <B>方法名称：batchInsert</B>
   * <B>概要说明：将每个表中域的信息插入到表中；</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年06月21日 09:06:39
   * @Param [msThirdPartyTableFieldsDos]
   **/
  private void batchInsert(LinkedList<MsThirdPartyTableFieldsDo> msThirdPartyTableFieldsDos) {
    try {
      msThirdPartyTableFieldsMapper.insertBatch(msThirdPartyTableFieldsDos);
    } catch (Exception e) {
      log.error("# MsThirdPartTableFieldsServiceImpl.batchInsert() # 将每个表有多少字段信息插入到数据库中出现了异常。", e);
    }
  }
}
