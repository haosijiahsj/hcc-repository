package com.hcc.repository.core.spring;

import com.hcc.repository.core.spring.support.MapperScanBeanRegistrar;
import org.springframework.context.annotation.Import;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * MapperScan
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
@Import(MapperScanBeanRegistrar.class)
public @interface EnableRepository {

    /**
     * 包扫描的路径
     * @return
     */
    String[] value() default {};

    /**
     * 包扫描的路径
     * @return
     */
    String[] basePackages() default {};

    /**
     * 数据源bean name
     * @return
     */
    String dataSourceRef() default "";

    /**
     * 配置bean name
     * @return
     */
    String configurationRef() default "";

}
