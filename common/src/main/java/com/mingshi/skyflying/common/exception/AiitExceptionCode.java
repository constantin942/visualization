package com.mingshi.skyflying.common.exception;

/**
 * @author 49090 2017年12月22日下午12:25:18
 * 本内容仅限于杭州玖河网络科技股份有限公司内部传阅，禁止外泄以及用于其他的商业目的
 * @类描述：@类AppExceptionCode.java 的实现描述：错误枚举类
 */
public enum AiitExceptionCode {

  SUCCESS("0000", "success", "成功"),
  FAILURE("1000", "failed", "失败"),
  ;

  /**
   * 错误码
   */
  private String code;

  /**
   * 错误中文描述信息
   */
  private String msgCn;

  /**
   * 错误英文描述信息
   */
  private String msgEn;

  private AiitExceptionCode(String code, String msgEn, String msgCn) {
    this.code=code;
    this.msgEn=msgEn;
    this.msgCn=msgCn;
  }

  public String getMsgCn() {
    return msgCn;
  }

  public String getCode() {
    return code;
  }

  public String getMsgEn() {
    return msgEn;
  }
}
