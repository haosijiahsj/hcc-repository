package com.hcc.repository.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * 逻辑删除注解<br/>
 * 配置拦截器后生效，列无法使用converter转换器，拦截器中未处理<br/>
 *
 * @author hushengjun
 * @date 2023/3/23
 */
@Documented
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface LogicDelete {

    /**
     * 逻辑未删除值
     * @return
     */
    String value();

    /**
     * 逻辑删除值
     * @return
     */
    String delValue() default "";

    /**
     * 删除的值类型，解决唯一索引的问题
     * @return
     */
    LogicDelValueType logicDelValueType() default LogicDelValueType.ASSIGNED;

}
