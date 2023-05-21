package com.hcc.repository.core.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 条件注解
 */
@Documented
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Condition {
    /**
     * SpEL表达式，执行后返回bool值，为空或为false不拼接该条件
     * @return
     */
    String exp() default "";

    /**
     * 条件sql
     * @return
     */
    String value();
}
