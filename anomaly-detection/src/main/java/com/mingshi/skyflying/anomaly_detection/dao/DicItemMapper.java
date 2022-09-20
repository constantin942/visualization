package com.mingshi.skyflying.anomaly_detection.dao;

import com.mingshi.skyflying.anomaly_detection.domain.DicItem;

import java.util.List;
import org.apache.ibatis.annotations.Param;

public interface DicItemMapper {

    int deleteByPrimaryKey(Integer id);

    int insert(DicItem record);

    int insertSelective(DicItem record);

    DicItem selectByPrimaryKey(Integer id);

    int updateByPrimaryKeySelective(DicItem record);

    int updateByPrimaryKey(DicItem record);

    List<DicItem> selectAll();
}