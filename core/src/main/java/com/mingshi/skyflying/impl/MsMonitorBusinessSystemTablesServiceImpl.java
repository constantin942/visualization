package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.domain.MsMonitorBusinessSystemTablesDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsMonitorBusinessSystemTablesMapper;
import com.mingshi.skyflying.init.LoadAllEnableMonitorTablesFromDb;
import com.mingshi.skyflying.service.MsMonitorBusinessSystemTablesService;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：MsMonitorBusinessSystemTablesServiceImpl</B>
 * <B>概要说明：操作所有业务系统的表</B>
 *
 * @Author zm
 * @Date 2022年07月13日 10:07:38
 * @Param
 * @return
 **/
@Slf4j
@Service("msMonitorBusinessSystemTablesService")
public class MsMonitorBusinessSystemTablesServiceImpl implements MsMonitorBusinessSystemTablesService {

  @Resource
  private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
  @Resource
  private MingshiServerUtil mingshiServerUtil;

  /**
   * <B>方法名称：getAllTables</B>
   * <B>概要说明：获取所有的表</B>
   *
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月13日 10:07:54
   * @Param []
   **/
  @Override
  public ServerResponse<String> getAllTables(String tableName, String dbName, String dbAddress, Integer pageNo, Integer pageSize) {
    log.info(" 开始执行 # MsMonitorBusinessSystemTablesServiceImpl.updateTableInformation() # 获取所有的表。");
    Map<String, Object> queryMap = new HashMap<>(Const.NUMBER_EIGHT);
    if (StringUtil.isNotBlank(tableName)) {
      tableName = tableName.trim().replace("\t", "");
      queryMap.put("tableName", tableName);
    }
    if (StringUtil.isNotBlank(dbName)) {
      dbName = dbName.trim().replace("\t", "");
      queryMap.put("dbName", dbName);
    }
    if (StringUtil.isNotBlank(dbAddress)) {
      dbAddress = dbAddress.trim().replace("\t", "");
      queryMap.put("dbAddress", dbAddress);
    }
    if (null == pageNo) {
      pageNo = 1;
    }
    if (null == pageSize) {
      pageSize = 10;
    }
    queryMap.put("pageNo", (pageNo - 1) * pageSize);
    queryMap.put("pageSize", pageSize);

    List<MsMonitorBusinessSystemTablesDo> msMonitorBusinessSystemTablesDos = msMonitorBusinessSystemTablesMapper.selectAllByQueryMap(queryMap);

    Integer count = msMonitorBusinessSystemTablesMapper.selectAllByQueryMapCount(queryMap);
    Map<String, Object> context = new HashMap<>(Const.NUMBER_EIGHT);
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    if (null != msMonitorBusinessSystemTablesDos && 0 < msMonitorBusinessSystemTablesDos.size()) {
      context.put("rows", JsonUtil.obj2String(msMonitorBusinessSystemTablesDos));
    }
    context.put("total", count);
    bySuccess.setData(JsonUtil.obj2String(context));

    log.info(" 执行完毕 # MsMonitorBusinessSystemTablesServiceImpl.updateTableInformation() # 获取所有的表。");
    return bySuccess;
  }

  /**
   * <B>方法名称：updateTableInformation</B>
   * <B>概要说明：更新表的启用状态</B>
   *
   * @param tableId
   * @param isDelete
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月13日 10:07:54
   * @Param []
   */
  @Override
  public ServerResponse<String> updateTableInformation(Integer tableId, Integer isDelete) {
    log.info(" 开始执行 # MsMonitorBusinessSystemTablesServiceImpl.updateTableInformation() # 更新表【{}】的状态值【{}】。", tableId, isDelete);
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo = msMonitorBusinessSystemTablesMapper.selectByPrimaryKey(tableId);
    if (null == msMonitorBusinessSystemTablesDo) {
      return ServerResponse.createByErrorMessage("表id在表中不存在", "");
    }
    if (msMonitorBusinessSystemTablesDo.getIsDelete().equals(isDelete)) {
      return bySuccess;
    }
    msMonitorBusinessSystemTablesDo.setIsDelete(isDelete);
    int updateResult = msMonitorBusinessSystemTablesMapper.updateByPrimaryKeySelective(msMonitorBusinessSystemTablesDo);
    if (1 != updateResult) {
      return ServerResponse.createByErrorMessage("更新失败", "");
    }
    log.info(" 执行完毕 # MsMonitorBusinessSystemTablesServiceImpl.updateTableInformation() # 更新表【{}】的状态值【{}】成功。", tableId, isDelete);

    // 在本地内存中更新表的状态；2022-07-13 13:57:54
    LoadAllEnableMonitorTablesFromDb.put(msMonitorBusinessSystemTablesDo.getTableName(), isDelete);
    return bySuccess;
  }

  /**
   * <B>方法名称：updateTableDesc</B>
   * <B>概要说明：更新表的描述信息</B>
   *
   * @param id
   * @param tableDesc
   * @return com.mingshi.skyflying.common.utils.response.ServerResponse<java.lang.String>
   * @Author zm
   * @Date 2022年07月21日 17:07:02
   * @Param [id, tableDesc]
   */
  @Override
  public ServerResponse<String> updateTableDesc(Integer id, String tableDesc) {
    try {
      MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo = msMonitorBusinessSystemTablesMapper.selectByPrimaryKey(id);
      if (null == msMonitorBusinessSystemTablesDo) {
        return ServerResponse.createByErrorMessage("表id不存在", "");
      }

      // 更新数据库中表的描述信息；
      msMonitorBusinessSystemTablesDo.setTableDesc(tableDesc);
      msMonitorBusinessSystemTablesMapper.updateByPrimaryKeySelective(msMonitorBusinessSystemTablesDo);

      // 更新本地内存中的表的描述信息；2022-07-21 17:20:53
      String tableName = mingshiServerUtil.getTableName(msMonitorBusinessSystemTablesDo);
      LoadAllEnableMonitorTablesFromDb.setTableDesc(tableName, tableDesc);
    } catch (Exception e) {
      log.error("# MsMonitorBusinessSystemTablesServiceImpl.updateTableDesc() # 根据id = 【{}】更新表的描述信息【{}】时，出现了异常。", id, tableDesc, e);
      return ServerResponse.createByErrorMessage("更新失败", "");
    }
    return ServerResponse.createBySuccess();
  }
}
