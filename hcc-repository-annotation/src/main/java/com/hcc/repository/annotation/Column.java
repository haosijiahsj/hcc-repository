package com.hcc.repository.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Column
 *
 * @author hushengjun
 * @date 2023/3/3
 */
@Documented
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Column {

    /**
     * 列名
     * @return
     */
    String value() default "";

    /**
     * 转换器
     * @return
     */
    Class<? extends IConverter> converter() default DefaultConverter.class;

    /**
     * 是否忽略该字段
     * @return
     */
    boolean ignore() default false;

}
