package com.mingshi.skyflying.utils;

import java.util.Map;

/**
 * <B>主类名称: MapUtil</B>
 * <B>概要说明：map类对应的util</B>
 * @Author zm
 * Date 2022/5/23 11:24
 *
 * @Version 1.0
 **/
public class MapUtil {
  /**
   * 根据map的value获取map的key
   * 注意：若value相同的值可能有很多个，要返回多个key值。
   *      就要把找到的key标记，下次不再用，一起返回。
   *
   * @param map   需要获取key的map
   * @param value 指定value
   * @return 返回key
   */
  public static String getKey(Map<String, String> map, String value) {
    // 初始化key
    String key = "";
    /**
     * 遍历map
     */
    for (Map.Entry<String, String> entry : map.entrySet()) {
      /**
       * 如果value和key对应的value相同 并且 key不在list中
       */
      if (value.equals(entry.getValue())) {
        key = entry.getKey();
        break;
      }
    }
    return key;
  }

}
