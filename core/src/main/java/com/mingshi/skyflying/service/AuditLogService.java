package com.mingshi.skyflying.service;

import com.aliyun.dms_enterprise20181101.models.ListSQLExecAuditLogResponseBody;
import com.mingshi.skyflying.domain.MsAuditLogDo;
import com.mingshi.skyflying.response.ServerResponse;

import java.util.List;

public interface AuditLogService extends ParentService<MsAuditLogDo, Long> {
  ServerResponse<String> process(List<ListSQLExecAuditLogResponseBody.ListSQLExecAuditLogResponseBodySQLExecAuditLogListSQLExecAuditLog> listSQLExecAuditLogList);
}
