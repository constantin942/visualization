package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.DicItem;
import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;

import java.util.ArrayList;
import java.util.List;

import com.mingshi.skyflying.common.response.ServerResponse;
import org.apache.ibatis.annotations.Param;

public interface PortraitConfigMapper {

    int deleteByPrimaryKey(Integer id);

    int insert(PortraitConfig record);

    int insertSelective(PortraitConfig record);

    PortraitConfig selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(PortraitConfig record);

    int updateByPrimaryKey(PortraitConfig record);

    PortraitConfig selectOne();

    void deleteOld();

    ArrayList<String> selectByName(String typeName);

    String selectOneByName(String demo_mode);

    void setDemoMode(String enable);
}