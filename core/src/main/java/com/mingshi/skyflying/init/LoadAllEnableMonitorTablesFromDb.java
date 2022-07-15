package com.mingshi.skyflying.init;

import com.mingshi.skyflying.dao.MsMonitorBusinessSystemTablesMapper;
import com.mingshi.skyflying.domain.MsMonitorBusinessSystemTablesDo;
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
 * Author zm
 * Date 2022/7/13 10:42
 *
 * @Version 1.0
 **/
@Component
public class LoadAllEnableMonitorTablesFromDb implements ApplicationRunner {
  @Resource
  private MsMonitorBusinessSystemTablesMapper msMonitorBusinessSystemTablesMapper;
  @Resource
  private MingshiServerUtil mingshiServerUtil;

  private static volatile Map<String, Integer> concurrentHashMap = new ConcurrentHashMap();

  private static volatile Map<String, Integer> isChangedMap = new ConcurrentHashMap();

  public static Map<String, Integer> getIsChangedMap() {
    return isChangedMap;
  }

  @Override
  public void run(ApplicationArguments args) throws Exception {
    doRun();
  }

  private void doRun() {
    List<MsMonitorBusinessSystemTablesDo> msMonitorBusinessSystemTablesDos = msMonitorBusinessSystemTablesMapper.selectAll();
    if (null != msMonitorBusinessSystemTablesDos && 0 < msMonitorBusinessSystemTablesDos.size()) {
      for (MsMonitorBusinessSystemTablesDo msMonitorBusinessSystemTablesDo : msMonitorBusinessSystemTablesDos) {
        String key = mingshiServerUtil.getTableName(msMonitorBusinessSystemTablesDo);
        concurrentHashMap.put(key, msMonitorBusinessSystemTablesDo.getIsDelete());
      }
    }
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
    Integer status = concurrentHashMap.get(tableName);
    if (null == status) {
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
    concurrentHashMap.put(tableName, isDelete);
  }
}
