package com.hcc.repository.core.spring;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;

/**
 * InjectMapperBeanPostProcessor
 *
 * @author hushengjun
 * @date 2023/3/3
 */
@Slf4j
public class InjectMapperBeanPostProcessor implements BeanPostProcessor {

    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
        return bean;
    }

}
