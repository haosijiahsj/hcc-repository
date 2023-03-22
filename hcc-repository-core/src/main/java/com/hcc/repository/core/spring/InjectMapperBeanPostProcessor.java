package com.hcc.repository.core.spring;

import com.hcc.repository.core.InjectMapper;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.proxy.InjectMapperProxyFactory;
import com.hcc.repository.core.utils.ReflectUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.jdbc.core.JdbcTemplate;

import java.lang.reflect.Field;

/**
 * InjectMapperBeanPostProcessor
 *
 * @author hushengjun
 * @date 2023/3/3
 */
@Slf4j
public class InjectMapperBeanPostProcessor implements BeanPostProcessor {

    private JdbcTemplate jdbcTemplate;

    public InjectMapperBeanPostProcessor(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
        Field[] fields = bean.getClass().getDeclaredFields();
        for (Field field : fields) {
            InjectMapper injectMapper = field.getAnnotation(InjectMapper.class);
            if (injectMapper == null) {
                continue;
            }
            Class<?> mapperClass = field.getType();
            if (!mapperClass.isInterface()) {
                log.warn("类：{}，不是接口，无法添加代理", mapperClass.getName());
                continue;
            }
            if (BaseMapper.class.isAssignableFrom(mapperClass)) {
                log.warn("类：{}，不是继承自BaseMapper，无法添加代理", mapperClass.getName());
                continue;
            }

            // 添加mapper的动态代理
            Object proxy = InjectMapperProxyFactory.create(mapperClass, jdbcTemplate);
            ReflectUtils.setValue(bean, field, proxy);
        }

        return bean;
    }

}
