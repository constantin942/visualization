package com.mingshi.skyflying.common.dao;

import com.mingshi.skyflying.common.domain.MsSegmentDetailDo;
import java.util.List;
import java.util.Map;

public interface MsSegmentDetailUsernameIsNullMapper {
    int deleteByPrimaryKey(Integer id);

    int deleteByIds(List<MsSegmentDetailDo> list);

    void insertSelectiveBatch(List<MsSegmentDetailDo> list);

    int insertSelective(MsSegmentDetailDo msSegmentDetailDo);

    MsSegmentDetailDo selectByPrimaryKey(Integer id);

    List<String> selectAllTokenUserNameIsNull();

    List<MsSegmentDetailDo> selectAllUserNameIsNotNull();

    List<String> selectAllGlobalTraceIdUserNameIsNull();

    int updateByPrimaryKeySelective(MsSegmentDetailDo msSegmentDetailDo);

    List<Map<String,String>> selectAllGlobalTraceIdUserNameIsNotNull();

    int updateUserNameByToken(Map<String,String> map);

    int updateUserNameByGlobalTraceId(Map<String,String> map);

    int updateBatch(List<Map<String,String>> mapList);
}