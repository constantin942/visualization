package com.mingshi.skyflying.common.aspect;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <B>方法名称：OperationAuditAspectAnnotation</B>
 * <B>概要说明：记录操作日志的注解；</B>
 * @Author zm
 * @Date 2022年09月20日 15:09:19
 * @Param
 * @return
 **/
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface OperationAuditAspectAnnotation {
    boolean isStart() default false;
}
