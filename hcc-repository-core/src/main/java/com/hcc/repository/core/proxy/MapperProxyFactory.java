package com.hcc.repository.core.proxy;

import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.ReflectUtils;

import javax.sql.DataSource;

/**
 * mapper代理工厂
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public class MapperProxyFactory {

    private MapperProxyFactory() {}

    /**
     * 创建代理
     * @param interfaceType
     * @param dataSource
     * @param configuration
     * @param <T>
     * @return
     */
    public static <T> T create(Class<T> interfaceType, DataSource dataSource, RepositoryConfiguration configuration) {
        Assert.isTrue(interfaceType != null, "mapper class不能为空");
        Assert.isTrue(dataSource != null, "数据源不能为空");
        Assert.isTrue(interfaceType.isInterface(), String.format("mapper class必须为接口，当前class: %s不是接口", interfaceType.getName()));


        // jdbcTemplateProxy代理创建
        JdbcTemplateProxyInvocationHandler jdbcTemplateProxyInvocationHandler
                = new JdbcTemplateProxyInvocationHandler(new JdbcTemplateWrapper(dataSource), configuration.getInterceptors());
        JdbcTemplateProxy jdbcTemplateProxy = ReflectUtils.newProxy(JdbcTemplateProxy.class, jdbcTemplateProxyInvocationHandler);

        // Mapper代理创建
        MapperMethodInvocationHandler invocationHandler = new MapperMethodInvocationHandler(jdbcTemplateProxy, interfaceType, configuration);

        return ReflectUtils.newProxy(interfaceType, invocationHandler);
    }

}
