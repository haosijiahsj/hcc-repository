package com.hcc.repository.core.annotation;

import com.hcc.repository.core.jdbc.BasicRowMapper;
import com.hcc.repository.core.jdbc.mapper.GeneralRowMapper;

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
     * sql
     * @return
     */
    String value();

    /**
     * 映射器
     * @return
     */
    Class<? extends BasicRowMapper> rowMapper() default GeneralRowMapper.class;

}
