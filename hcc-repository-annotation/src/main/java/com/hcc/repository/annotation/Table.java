package com.hcc.repository.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Table
 *
 * @author hushengjun
 * @date 2023/3/3
 */
@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Table {

    /**
     * 表名
     * @return
     */
    String value() default "";

    /**
     * 列名统一前缀
     * @return
     */
    String columnPrefix() default "";

    /**
     * 忽略的属性名称，实体字段名称
     * @return
     */
    String[] ignorePropNames() default {};

    /**
     * 属性设置监听器
     * @return
     */
    Class<? extends PropSetListener> propSet() default Constants.UnknownPropSetListener.class;

}
