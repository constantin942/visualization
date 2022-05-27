package com.mingshi.skyflying.domain;

import lombok.Data;

/**
 * <B>主类名称: AllAuditLogDo</B>
 * <B>概要说明：</B>
 * Author zm
 * Date 2022/5/26 19:44
 *
 * @Version 1.0
 **/
@Data
public class AllAuditLogDo {
  // LOG	DB	TID	USER	USER_IP	SQL_TYPE	FAIL	CHECK_ROWS	UPDATE_ROWS	LATENCY	TS	ORIGIN_TIME
  private String LOG;
  private String DB;
  private long TID;
  private String USER;
  private String USER_IP;
  private String SQL_TYPE;
  private String FAIL;
  private int CHECK_ROWS;
  private int UPDATE_ROWS;
  private int LATENCY;
  private String TS;
  private String ORIGIN_TIME;
}
