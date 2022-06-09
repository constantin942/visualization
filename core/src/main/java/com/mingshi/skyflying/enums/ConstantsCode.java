package com.mingshi.skyflying.enums;

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
  ;

  /**
   * 错误中文描述信息
   */
  private String msgCn;

  /**
   * 错误英文描述信息
   */
  private String msgEn;

  private ConstantsCode(String msgEn, String msgCn) {
    this.msgEn = msgEn;
    this.msgCn = msgCn;
  }

  public String getMsgCn() {
    return msgCn;
  }

  public String getMsgEn() {
    return msgEn;
  }
}
