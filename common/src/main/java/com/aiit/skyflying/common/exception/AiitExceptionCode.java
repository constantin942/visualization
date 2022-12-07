package com.aiit.skyflying.common.exception;

/**
 * @author 49090 2017年12月22日下午12:25:18
 * 本内容仅限于杭州玖河网络科技股份有限公司内部传阅，禁止外泄以及用于其他的商业目的
 * @类描述：@类AppExceptionCode.java 的实现描述：错误枚举类
 */
public enum AiitExceptionCode {

  /**
   * 操作成功
   */
  SUCCESS("0000", "success", "成功"),

  /**
   * 操作失败
   */
  FAILURE("1000", "failed", "失败"),

  FIREWALL_RULES_RULES_LAST_RECORD_NULL("1001", "failed", "规则总表中，不存在防火墙规则。"),
  FIREWALL_RULES_RULES_LAST_RECORD_NOT_DEFAULT("1002", "failed", "规则总表中，最后一条规则不是默认的规则。"),
  FIREWALL_RULES_RULES_DETAIL_LAST_RECORD_NULL("1003", "failed", "规则详细表中，不存在防火墙规则。"),
  FIREWALL_RULES_DETAIL_RULES_LAST_RECORD_NOT_DEFAULT("1004", "failed", "规则详细表中，源IP和目的IP不是ANY。"),
  SOURCE_DESTINATION_PORT_NULL("1005", "failed", "源端口号和目的端口号都为null。"),
  SOURCE_PORT_NULL_DESTINATION_PORT_NOT_NULL("1006", "failed", "源端口号为null，目的端口号不为null。"),
  SOURCE_PORT_NOT_NULL_DESTINATION_PORT_NULL("1007", "failed", "源端口号不为null，目的端口号为null。"),
  USER_IS_NOT_LOGGED_IN( "1008", "user is not loggined in", "未登录"),
  USERNAME_IS_EMPTY( "1009", "userName is empty", "用户名不能为空"),
  PASSWORD_IS_EMPTY( "1010", "password is empty", "密码不能为空"),
  USERNAME_DOES_NOT_EXIST( "1011", "user does not exist", "用户不存在"),
  OLD_PASSWORD_IS_EMPTY( "1012", "old password is empty", "旧密码不能为空"),
  NEW_PASSWORD_IS_EMPTY( "1013", "new password is empty", "新密码不能为空"),
  PASSWORD_IS_SAME( "1014", "old password and  new passowrd are same", "新密码和旧密码不能一样"),
  NEW_PASSWORD_TOO_LONG( "1015", "password too long", "新密码长度太长"),
  OLD_PASSWORD_IS_ERROR( "1016", "old password is error", "输入的旧密码不对"),
  INCORRECT_PASSWORD( "1017", "incorrect password", "密码错误"),
  PASSWORD_ERROR_MORE_THAN_FIVE_TIMES( "1018", "Password error more than 5 times, please try again in an hour", "密码错误次数超过5次，请一小时以后再试"),
  USERNAME_ROLE_NOT_EXIST( "1020", "user rule not exist", "用户角色不存在"),
  USERNAME_ROLE_MENU_NOT_EXIST( "1021", "user rule menu not exist", "用户角色对应的菜单不存在"),
  USE_INSERT_FAILED( "1022", "user insert failed", "保存用户数据失败"),
  NOT_FIND_DATA("1033","not find data","未找到符合要求的数据"),
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
