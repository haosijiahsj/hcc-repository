package com.hcc.repository.core.spring.support;

import lombok.Setter;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.util.StringUtils;

/**
 * Mapper的BeanDefinition注册器
 *
 * @author hushengjun
 * @date 2023/4/5
 */
public class MapperBeanDefinitionRegistryPostProcessor implements BeanDefinitionRegistryPostProcessor {

    @Setter
    private String basePackage;
    @Setter
    private String dataSourceRef;
    @Setter
    private String configurationRef;

    @Override
    public void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry) throws BeansException {
        ClassPathMapperScanner scanner = new ClassPathMapperScanner(registry);
        scanner.setDataSourceRef(dataSourceRef);
        scanner.setConfigurationRef(configurationRef);
        scanner.registerFilters();

        scanner.scan(StringUtils.tokenizeToStringArray(basePackage, ConfigurableApplicationContext.CONFIG_LOCATION_DELIMITERS));
    }

    @Override
    public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {}

}
