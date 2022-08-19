package com.mingshi.skyflying.impl;

import com.mingshi.skyflying.common.domain.MsThirdPartyTableListDo;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.utils.JsonUtil;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsSegmentDetailDao;
import com.mingshi.skyflying.dao.MsThirdPartyTableListMapper;
import com.mingshi.skyflying.service.MsThirdPartyTableListService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * <B>方法名称：MsThirdPartTableListServiceImpl</B>
 * <B>概要说明：获取数据库中有哪些表</B>
 *
 * @Author zm
 * @Date 2022年06月20日 14:06:48
 * @Param
 * @return
 **/
@Slf4j
@Service("msThirdPartyTableListService")
public class MsThirdPartTableListServiceImpl implements MsThirdPartyTableListService {

  @Resource
  private MsSegmentDetailDao msSegmentDetailDao;
  @Resource
  private MsThirdPartyTableListMapper msThirdPartyTableListMapper;

  @Override
  public ServerResponse<String> getAllTableNames(String dbName) {
    log.info("开始执行 # MsThirdPartTableListServiceImpl.getAllTableNames() # 根据数据库名称获取这个数据库中所有的表。");
    Map<String, String> dbNameTableMap = new HashMap<>(Const.NUMBER_EIGHT);

    List<MsThirdPartyTableListDo> msThirdPartyTableListDoList = new LinkedList<>();
    // 如果前端没有传递数据库名过来，那么从调用链表ms_segment_detail中获取所有的数据库名称。2022-06-20 14:40:40
    if (StringUtil.isBlank(dbName)) {
      List<String> list = msSegmentDetailDao.selectAllInstanceName();
      for (int i = 0; i < list.size(); i++) {
        dbName = list.get(i);
        // 根据数据库名称，获取这个数据库有哪些表；2022-06-20 14:44:27
        List<String> tableList = msThirdPartyTableListMapper.selectAllTables(dbName);
        if (0 < tableList.size()) {
          dbNameTableMap.put(dbName, JsonUtil.obj2String(tableList));
          for (String tableName : tableList) {
            // 将这个数据库中对应的表都插入到数据库中；
            MsThirdPartyTableListDo msThirdPartyTableListDo = new MsThirdPartyTableListDo();
            msThirdPartyTableListDo.setThirdPartyDbName(dbName);
            msThirdPartyTableListDo.setThirdPartyTableName(tableName);
            msThirdPartyTableListDoList.add(msThirdPartyTableListDo);
          }
        }
      }
    }
    // 将数据库对应的所有的表插入到数据库中；2022-06-20 18:02:53
    batchInsert(msThirdPartyTableListDoList);
    ServerResponse<String> bySuccess = ServerResponse.createBySuccess();
    if (0 < dbNameTableMap.size()) {
      bySuccess.setData(JsonUtil.obj2String(dbNameTableMap));
      log.info("开始执行 # MsThirdPartTableListServiceImpl.getAllTableNames() # 根据数据库名称获取这个数据库中所有的表。获取到的数据库名称和对应的数据库表有【{}】。", JsonUtil.obj2String(dbNameTableMap));
    }
    log.info("执行结束 # MsThirdPartTableListServiceImpl.getAllTableNames() # 根据数据库名称获取这个数据库中所有的表。");
    return bySuccess;
  }

  /**
   * <B>方法名称：batchInsert</B>
   * <B>概要说明：批量插入</B>
   * @Author zm
   * @Date 2022年06月20日 17:06:14
   * @Param [msThirdPartyTableListDoList]
   * @return void
   **/
  private void batchInsert(List<MsThirdPartyTableListDo> msThirdPartyTableListDoList) {
    try {
      msThirdPartyTableListMapper.insertBatch(msThirdPartyTableListDoList);
    } catch (Exception e) {
      log.error(" # MsThirdPartTableListServiceImpl.batchInsert() # 将数据库中所有的表信息插入到数据库中出现了异常。");
    }
  }
}
