package com.hcc.repository.core.spring.support;

import com.hcc.repository.core.proxy.MapperProxyFactory;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

import javax.sql.DataSource;
import java.util.Optional;

/**
 * MapperFactoryBean
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Slf4j
public class MapperFactoryBean<T> implements FactoryBean<T>, InitializingBean {

    private final Class<T> mapperClass;

    @Setter
    private DataSource dataSource;
    @Setter
    private RepositoryConfiguration configuration;

    public MapperFactoryBean(Class<T> mapperClass) {
        this.mapperClass = mapperClass;
    }

    @Override
    public T getObject() throws Exception {
        configuration = Optional.ofNullable(configuration).orElse(new RepositoryConfiguration());
        T mapperProxy = MapperProxyFactory.create(mapperClass, dataSource, configuration);

        if (log.isDebugEnabled()) {
            log.debug("mapper: {} 代理创建完成", mapperClass.getName());
        }

        return mapperProxy;
    }

    @Override
    public Class<?> getObjectType() {
        return mapperClass;
    }

    @Override
    public void afterPropertiesSet() throws Exception {
    }

}
