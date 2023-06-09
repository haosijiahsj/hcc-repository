package com.hcc.repository.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Id
 *
 * @author hushengjun
 * @date 2023/3/3
 */
@Documented
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Id {

    String value() default "";

    IdType idType() default IdType.AUTO;

    Class<? extends IdGenerator> generator() default Constants.UnknownIdGenerator.class;

    boolean useSingletonIdGenerator() default true;

}
