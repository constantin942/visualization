package com.mingshi.skyflying.service;

import com.mingshi.skyflying.common.domain.MsConfigDo;
import com.mingshi.skyflying.common.response.ServerResponse;

/**
 * <B>类名称：MsConfigService</B>
 * <B>概要说明：</B>
 *
 * @Author zm
 * @Date 2022/10/11 11:20
 **/
public interface MsConfigService extends ParentService<MsConfigDo, Long> {
    ServerResponse<String> getAkSkFromDb();

    ServerResponse<String> setAkSkIntoDb(String ak, String sk);

    ServerResponse<String> getRegionFromDb();

    ServerResponse<String> setRegionIntoDb(String region);
}
