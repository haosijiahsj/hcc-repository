package com.hcc.repository.core.proxy;

import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.util.Assert;

import javax.sql.DataSource;

/**
 * InjectMapperProxyFactory
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public class InjectMapperProxyFactory {

    private InjectMapperProxyFactory() {}

    public static <T> T create(Class<T> interfaceType, JdbcTemplate jdbcTemplate) {
        Assert.isTrue(interfaceType.isInterface(), String.format("just support interface ! '%s' is not an interface !", interfaceType.getName()));
        Assert.isTrue(jdbcTemplate != null, "'jdbcTemplate' can't be null !");

        InjectMapperInvocationHandler invocationHandler = new InjectMapperInvocationHandler(new JdbcTemplateWrapper(jdbcTemplate), interfaceType);

        return ReflectUtils.newProxy(interfaceType, invocationHandler);
    }

    public static <T> T create(Class<T> interfaceType, DataSource dataSource) {
        Assert.isTrue(interfaceType.isInterface(), String.format("just support interface ! '%s' is not an interface !", interfaceType.getName()));
        Assert.isTrue(dataSource != null, "'jdbcTemplate' can't be null !");

        InjectMapperInvocationHandler invocationHandler = new InjectMapperInvocationHandler(new JdbcTemplateWrapper(dataSource), interfaceType);

        return ReflectUtils.newProxy(interfaceType, invocationHandler);
    }

}
