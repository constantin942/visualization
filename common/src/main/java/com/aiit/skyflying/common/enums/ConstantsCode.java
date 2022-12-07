package com.aiit.skyflying.common.enums;

/**
 * <B>方法名称：ConstantsCode</B>
 * <B>概要说明：常量</B>
 *
 * @Author zm
 * @Date 2022年06月07日 17:06:55
 * @Param
 * @return
 **/
public enum ConstantsCode {
  // 描述用户基于时间的访问次数；2022-06-07 16:09:34
  USER_PORTRAIT_NIGHT("night", "晚上"),
  USER_PORTRAIT_FORENOON("forenoon", "中午"),
  USER_PORTRAIT_AFTERNOON("afternoon", "下午"),

  USER_PORTRAIT_RULE_VISITED_TIME("user_visited_time", "基于访问时间段的告警规则：即若某用户通常白天访问数据，则夜间为异常；"),
  USER_PORTRAIT_RULE_VISITED_TABLE("user_visited_table", "基于访问过的表的告警规则：若某用户访问从未访问过的表时，则给出告警；"),
  ;

  /**
   * 错误中文描述信息
   */
  private String code;

  /**
   * 错误英文描述信息
   */
  private String desc;

  private ConstantsCode(String code, String desc) {
    this.desc = desc;
    this.code = code;
  }

  public String getCode() {
    return code;
  }

  public String getDesc() {
    return desc;
  }
}
