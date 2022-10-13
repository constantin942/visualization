package com.mingshi.skyflying.init;

import com.mingshi.skyflying.common.constant.Const;
import com.mingshi.skyflying.common.domain.MsMonitorBusinessSystemTablesDo;
import com.mingshi.skyflying.common.utils.StringUtil;
import com.mingshi.skyflying.dao.MsMonitorBusinessSystemTablesMapper;
import com.mingshi.skyflying.utils.MingshiServerUtil;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <B>主类名称: LoadAllEnableMonitorTablesFromDb</B>
 * <B>概要说明：项目启动，从数据库中加载所有处于禁用状态被监管的表</B>
 *
 * @Author zm
 * Date 2022/7/13 10:42
 * @Version 1.0
 **/
@Component
public class LoadAllEnableMonitorTablesFromDb implements ApplicationRunner {
  @Resource
  private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
  @Resource
  private MingshiServerUtil mingshiServerUtil;

  private static volatile Map<String, Integer> concurrentHashMapIsDelete = new ConcurrentHashMap<>();

  private static volatile Map<String, String> concurrentHashMapTableDesc = new ConcurrentHashMap<>();

  private static volatile Map<String, Integer> isChangedMap = new ConcurrentHashMap<>();

  public static Map<String, Integer> getIsChangedMap() {
    return isChangedMap;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    doRun();
  }

  private void doRun() {
    List<MsMonitorBusinessSystemTablesDo> msMonitorBusinessSystemTablesDos = msMonitorBusinessSystemTablesMapper.selectAll();
    if (null != msMonitorBusinessSystemTablesDos && !msMonitorBusinessSystemTablesDos.isEmpty()) {
      for (MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo : msMonitorBusinessSystemTablesDos) {
        String key = mingshiServerUtil.getTableName(msMonitorBusinessSystemTablesDo);
        concurrentHashMapIsDelete.put(key, msMonitorBusinessSystemTablesDo.getIsDelete());
        setTableDesc(key, msMonitorBusinessSystemTablesDo.getTableDesc());
      }
    }
  }

  /**
   * <B>方法名称：getTableDesc</B>
   * <B>概要说明：获取表的描述信息</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年07月21日 16:07:50
   * @Param [key]
   **/
  public static String getTableDesc(String key) {
    if (key.contains(Const.POUND_KEY)) {
      String[] split = key.split(Const.POUND_KEY);
      String peer = split[0];
      String dbName = split[1];
      String tableName = split[2];
      String tableDesc = "";
      if (tableName.contains(Const.EN_COMMA)) {
        String[] split1 = tableName.split(Const.EN_COMMA);
        for (String str : split1) {
          tableDesc = getEveryTableDesc(peer, dbName, str, tableDesc);
        }
      }else{
        tableDesc = getEveryTableDesc(peer, dbName, tableName, tableDesc);
      }
      return tableDesc;
    } else {
      return concurrentHashMapTableDesc.get(key);
    }
  }

  /**
   * <B>方法名称：getEveryTableDesc</B>
   * <B>概要说明：获取表的中文名称</B>
   * @Author zm
   * @Date 2022年09月23日 10:09:27
   * @Param [peer, dbName, str, tableDesc]
   * @return java.lang.String
   **/
  private static String getEveryTableDesc(String peer, String dbName, String str, String tableDesc) {
    String newTableName = peer + Const.POUND_KEY + dbName + Const.POUND_KEY + str;
    if (StringUtil.isBlank(tableDesc)) {
      tableDesc = concurrentHashMapTableDesc.get(newTableName);
      if (StringUtil.isBlank(tableDesc)) {
        tableDesc = str;
      }
    } else {
      String tableNameDesc = concurrentHashMapTableDesc.get(newTableName);
      if (StringUtil.isBlank(tableNameDesc)) {
        tableDesc = tableDesc + Const.POUND_KEY + str;
      } else {
        tableDesc = tableDesc + Const.POUND_KEY + tableNameDesc;
      }
    }
    return tableDesc;
  }

  /**
   * <B>方法名称：setTableDesc</B>
   * <B>概要说明：设置</B>
   *
   * @return java.lang.String
   * @Author zm
   * @Date 2022年07月21日 16:07:04
   * @Param [key, tableDesc]
   **/
  public static void setTableDesc(String key, String tableDesc) {
    if (StringUtil.isBlank(tableDesc)) {
      return;
    }
    concurrentHashMapTableDesc.put(key, tableDesc);
  }

  /**
   * <B>方法名称：getEnableStatus</B>
   * <B>概要说明：获取表的启用状态</B>
   *
   * @return java.lang.Boolean
   * @Author zm
   * @Date 2022年07月13日 11:07:30
   * @Param [tableName]
   **/
  public static Integer getTableEnableStatus(String tableName) {
    Integer status = concurrentHashMapIsDelete.get(tableName);
    if (null == status && !isChangedMap.containsKey(tableName)) {
      // 当获取表名时，如果当前表不在本地内存中，那么就将其插入到本地内存中。2022-07-13 14:03:14
        isChangedMap.put(tableName, 0);
    }
    return status;
  }

  /**
   * <B>方法名称：put</B>
   * <B>概要说明：往map中存放数据</B>
   *
   * @return void
   * @Author zm
   * @Date 2022年07月13日 11:07:13
   * @Param [tableName, isDelete]
   **/
  public static void put(String tableName, Integer isDelete) {
    concurrentHashMapIsDelete.put(tableName, isDelete);
  }
}
