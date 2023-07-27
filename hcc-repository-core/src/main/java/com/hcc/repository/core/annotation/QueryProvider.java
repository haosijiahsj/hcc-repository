package com.hcc.repository.core.annotation;

import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.jdbc.mapper.DefaultResultMapper;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 查询注解
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Documented
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface QueryProvider {

    /**
     * provider class类型
     * @return
     */
    Class<?> value() default void.class;
    /**
     * provider class类型 alias for value
     * @return
     */
    Class<?> type() default void.class;

    /**
     * 方法名称，默认与mapper中的标记此注解方法名称一致
     * @return
     */
    String method() default "";

    /**
     * 映射器
     * @return
     */
    Class<? extends ResultMapper> resultMapper() default DefaultResultMapper.class;

}
