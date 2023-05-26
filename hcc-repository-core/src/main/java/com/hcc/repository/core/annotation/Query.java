package com.hcc.repository.core.annotation;

import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.jdbc.mapper.GeneralResultMapper;

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
public @interface Query {

    /**
     * 主sql
     * @return
     */
    String value();

    /**
     * join的动态条件，适用于拼接LEFT JOIN, INNER JOIN等
     * @return
     */
    Condition[] joinConditions() default {};

    /**
     * 动态条件sql
     * @return
     */
    Condition[] conditions() default {};

    /**
     * 后缀sql
     * @return
     */
    String last() default "";

    /**
     * 映射器
     * @return
     */
    Class<? extends ResultMapper> resultMapper() default GeneralResultMapper.class;

}
