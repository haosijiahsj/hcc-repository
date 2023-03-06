package com.hcc.repository.core.proxy;

import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.util.Assert;

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

        InjectMapperInvocationHandler invocationHandler = new InjectMapperInvocationHandler(jdbcTemplate);

        return ReflectUtils.newProxy(interfaceType, invocationHandler);
    }

}
