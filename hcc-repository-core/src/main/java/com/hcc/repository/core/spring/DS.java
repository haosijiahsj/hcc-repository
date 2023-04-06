package com.hcc.repository.core.spring;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 动态数据源注解
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface DS {

    /**
     * 数据源名称
     * @return
     */
    String value() default "";

}
