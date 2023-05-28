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
     * 动态条件sql，注意Condition表达式一旦计算为true则直接拼接sql片段<br/>
     * 当AND或OR前出现WHERE关键字时，将去除该sql片段首部的AND或OR
     * @return
     */
    Condition[] conditions() default {};

    /**
     * 后缀sql，建议写LIMIT等无条件拼接的sql片段
     * @return
     */
    String last() default "";

    /**
     * 映射器
     * @return
     */
    Class<? extends ResultMapper> resultMapper() default GeneralResultMapper.class;

}
