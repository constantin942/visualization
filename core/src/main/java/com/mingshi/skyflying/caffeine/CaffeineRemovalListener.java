package com.mingshi.skyflying.caffeine;

import com.github.benmanes.caffeine.cache.RemovalCause;
import com.github.benmanes.caffeine.cache.RemovalListener;
import lombok.extern.slf4j.Slf4j;

/**
 * <B>主类名称: CaffeineRemovalListener</B>
 * <B>概要说明：使用Caffeine实例时，移除元素的监听者类</B>
 * Author zm
 * Date 2022/8/1 10:32
 *
 * @Version 1.0
 **/
@Slf4j
public class CaffeineRemovalListener implements RemovalListener<String,String>{
  @Override
  public void onRemoval(@org.checkerframework.checker.nullness.qual.Nullable String key, @org.checkerframework.checker.nullness.qual.Nullable String value, RemovalCause removalCause) {
    log.error("# CaffeineRemovalListener.onRemoval() # 要将Caffine实例中的 Key =【%s】 ，值 = 【%s】移除，其原因是 【%s】。", key, value, removalCause);
  }
}
