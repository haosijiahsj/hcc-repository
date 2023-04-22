package com.hcc.repository.core.spring.support;

import com.hcc.repository.core.spring.EnableRepository;
import com.hcc.repository.core.utils.StrUtils;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Mapper注册器
 *
 * @author shengjun.hu
 * @date 2021/6/15
 */
public class MapperScanBeanRegistrar implements ImportBeanDefinitionRegistrar {

    @Override
    public void registerBeanDefinitions(AnnotationMetadata annotationMetadata, BeanDefinitionRegistry beanDefinitionRegistry) {
        // 获取MapperScan注解信息
        AnnotationAttributes mapperScanAttrs
                = AnnotationAttributes.fromMap(annotationMetadata.getAnnotationAttributes(EnableRepository.class.getName()));
        if (mapperScanAttrs == null) {
            throw new IllegalArgumentException("无法找到MapperScan注解信息");
        }

        List<String> basePackages = new ArrayList<>();
        basePackages.addAll(
                Arrays.stream(mapperScanAttrs.getStringArray("value"))
                        .filter(StrUtils::isNotEmpty)
                        .collect(Collectors.toList())
        );
        basePackages.addAll(
                Arrays.stream(mapperScanAttrs.getStringArray("basePackages"))
                        .filter(StrUtils::isNotEmpty)
                        .collect(Collectors.toList())
        );

        BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(MapperBeanDefinitionRegistryPostProcessor.class);
        // 向下的传递的注解属性相关信息
        builder.addPropertyValue("basePackage", StrUtils.join(",", basePackages));
        String dataSourceRef = mapperScanAttrs.getString("dataSourceRef");
        if (StrUtils.isNotEmpty(dataSourceRef)) {
            builder.addPropertyValue("dataSourceRef", dataSourceRef);
        }
        String configurationRef = mapperScanAttrs.getString("configurationRef");
        if (StrUtils.isNotEmpty(configurationRef)) {
            builder.addPropertyValue("configurationRef", configurationRef);
        }

        String beanName = annotationMetadata.getClassName()
                + "#"
                + MapperScanBeanRegistrar.class.getSimpleName()
                + "#"
                + 0;

        // 向容器注入bean
        beanDefinitionRegistry.registerBeanDefinition(beanName, builder.getBeanDefinition());
    }

}
