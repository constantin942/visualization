package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.MsAlarmInformationDo;

import java.util.List;

public interface MsAlarmInformationMapper {

    int insertSelective(MsAlarmInformationDo record);

    MsAlarmInformationDo selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(MsAlarmInformationDo record);

    void insertSelectiveBatch(List<MsAlarmInformationDo> msAlarmInformationDoLinkedListist);
}
