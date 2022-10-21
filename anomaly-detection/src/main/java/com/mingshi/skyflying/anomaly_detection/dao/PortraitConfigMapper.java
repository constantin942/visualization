package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.PortraitConfig;

import java.util.ArrayList;

public interface PortraitConfigMapper {

    int deleteByPrimaryKey(Integer id);

    int insert(PortraitConfig portraitConfig);

    int insertSelective(PortraitConfig portraitConfig);

    PortraitConfig selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(PortraitConfig portraitConfig);

    int updateByPrimaryKey(PortraitConfig portraitConfig);

    PortraitConfig selectOne();

    void deleteOld();

    ArrayList<String> selectByName(String typeName);

    String selectOneByName(String demoMode);

    void setDemoMode(String enable);
}
