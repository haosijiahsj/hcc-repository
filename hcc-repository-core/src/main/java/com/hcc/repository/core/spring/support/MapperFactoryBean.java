package com.hcc.repository.core.spring.support;

import com.hcc.repository.core.proxy.InjectMapperProxyFactory;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import lombok.Setter;
import org.springframework.beans.factory.FactoryBean;

import javax.sql.DataSource;
import java.util.Collections;

/**
 * MapperFactoryBean
 *
 * @author hushengjun
 * @date 2023/4/5
 */
public class MapperFactoryBean<T> implements FactoryBean<T> {

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
        if (configuration == null) {
            configuration = new RepositoryConfiguration();
            configuration.setInterceptors(Collections.emptyList());
        }
        return InjectMapperProxyFactory.create(mapperClass, dataSource, configuration);
    }

    @Override
    public Class<?> getObjectType() {
        return mapperClass;
    }

}
