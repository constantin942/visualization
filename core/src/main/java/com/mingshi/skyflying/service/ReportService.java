package com.mingshi.skyflying.service;


import com.mingshi.skyflying.common.domain.MsReport;
import com.mingshi.skyflying.common.domain.MsUserFrom;
import com.mingshi.skyflying.common.response.ServerResponse;
import com.mingshi.skyflying.common.service.ParentService;

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

