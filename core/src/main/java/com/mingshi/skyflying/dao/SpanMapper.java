package com.mingshi.skyflying.dao;

import com.mingshi.skyflying.domain.Span;

import java.util.List;

public interface SpanMapper {

  int insertSelective(Span record);

  Span selectByPrimaryKey(Integer id);

  void insertSelectiveBatch(List<Span> list);

  int updateByPrimaryKeySelective(Span record);

}
