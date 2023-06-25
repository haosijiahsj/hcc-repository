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
public @interface Query {

    /**
     * 主sql
     * @return
     */
    String value();

    /**
     * 动态条件sql，注意Condition表达式为空或计算为true则直接拼接sql片段<br/>
     * 当AND或OR前出现WHERE关键字时，将去除该sql片段首部的AND或OR<br/>
     * 当Update的SET子句最后一个为逗号结尾则会去掉逗号<br/>
     * 其它情况直接拼接
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
    Class<? extends ResultMapper> resultMapper() default DefaultResultMapper.class;

}
