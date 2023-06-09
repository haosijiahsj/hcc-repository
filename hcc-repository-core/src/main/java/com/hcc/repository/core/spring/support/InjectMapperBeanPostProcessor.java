package com.hcc.repository.core.spring.support;

import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.proxy.MapperProxyFactory;
import com.hcc.repository.core.spring.DS;
import com.hcc.repository.core.spring.InjectMapper;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import javax.sql.DataSource;
import java.lang.reflect.Field;

/**
 * InjectMapperBeanPostProcessor
 *
 * @author hushengjun
 * @date 2023/3/3
 */
@Slf4j
public class InjectMapperBeanPostProcessor implements BeanPostProcessor, ApplicationContextAware {

    private ApplicationContext applicationContext;

    private DataSource dataSource;
    @Setter
    private RepositoryConfiguration configuration;

    public InjectMapperBeanPostProcessor(DataSource dataSource) {
        this.dataSource = dataSource;
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

            DS ds = ReflectUtils.getAnnotation(mapperClass, DS.class);
            if (ds != null && StrUtils.isNotEmpty(ds.value())) {
                dataSource = applicationContext.getBean(DataSource.class, ds.value());
            }

            // 添加mapper的动态代理
            Object proxy = MapperProxyFactory.create(mapperClass, dataSource, configuration);
            ReflectUtils.setValue(bean, field, proxy);
        }

        return bean;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

}
