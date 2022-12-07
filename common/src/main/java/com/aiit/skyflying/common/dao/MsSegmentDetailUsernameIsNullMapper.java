package com.aiit.skyflying.common.dao;

import com.aiit.skyflying.common.domain.MsSegmentDetailDo;
import java.util.List;
import java.util.Map;

public interface MsSegmentDetailUsernameIsNullMapper {
    int deleteByIds(List<MsSegmentDetailDo> list);

    void insertSelectiveBatch(List<MsSegmentDetailDo> list);

    List<String> selectAllTokenUserNameIsNull();

    List<MsSegmentDetailDo> selectAllUserNameIsNotNull();

    List<Map<String,String>> selectAllGlobalTraceIdUserNameIsNotNull();

    int updateUserNameByToken(Map<String,String> map);

    int updateUserNameByGlobalTraceId(Map<String,String> map);

    int updateBatch(List<Map<String,String>> mapList);

    int updateNoUserName(List<Map<String,String>> mapList);

    List<Map<String,String>> selectAllNoUserNameBeforeSixHours(String gmtModified);
}
