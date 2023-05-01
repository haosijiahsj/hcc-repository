package com.hcc.repository.core.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Param
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Documented
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
public @interface Param {

    /**
     * 参数名称，参数类型为Map无需设定，其它必须设定
     * @return
     */
    String value() default "";

}
