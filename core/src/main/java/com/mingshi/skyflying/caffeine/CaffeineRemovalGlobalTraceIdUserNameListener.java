package com.mingshi.skyflying.caffeine;

import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import lombok.extern.slf4j.Slf4j;

/**
 * <B>主类名称: CaffeineRemovalGlobalTraceIdUserNameListener</B>
 * <B>概要说明：使用Caffeine实例时，移除元素的监听者类</B>
 * @Author zm
 * Date 2022/8/1 10:32
 *
 * @Version 1.0
 **/
@Slf4j
public class CaffeineRemovalGlobalTraceIdUserNameListener implements RemovalListener<String,String>{
  @Override
  public void onRemoval(@org.checkerframework.checker.nullness.qual.Nullable String key, @org.checkerframework.checker.nullness.qual.Nullable String value, RemovalCause removalCause) {
    if(!removalCause.equals(RemovalCause.REPLACED)){
        // ignore
    }
  }
}
