package com.aiit.skyflying.service;


import com.aiit.skyflying.common.domain.MsReport;
import com.aiit.skyflying.common.domain.MsUserFrom;
import com.aiit.skyflying.common.response.ServerResponse;
import com.aiit.skyflying.common.service.ParentService;

public interface ReportService extends ParentService<MsReport, Long> {

    /**
     * 生成报告
     * @return
     */
	ServerResponse<String> generateReport();

    /**
     * 获取最新的报告
     * @return
     */
	ServerResponse<String> getLatestReport();

}

