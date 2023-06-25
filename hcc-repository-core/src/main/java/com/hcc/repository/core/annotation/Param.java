package com.hcc.repository.core.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 参数注解，指定参数名称
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Documented
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
public @interface Param {

    /**
     * 参数名称，不设置时自动使用方法名称（在编译时设置了-parameters参数时生效）
     * @return
     */
    String value() default "";

}
