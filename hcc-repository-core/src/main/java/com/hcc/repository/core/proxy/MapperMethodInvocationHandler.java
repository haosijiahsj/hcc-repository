package com.hcc.repository.core.proxy;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.MethodHandlerFactory;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.utils.MethodHandlesUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.invoke.MethodHandle;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * mapper方法代理执行器
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public class MapperMethodInvocationHandler implements InvocationHandler {

    private final JdbcOperations jdbcOperations;
    private final Class<?> baseMapperClass;
    private final RepositoryConfiguration configuration;

    public MapperMethodInvocationHandler(JdbcOperations jdbcOperations, Class<?> baseMapperClass, RepositoryConfiguration configuration) {
        this.jdbcOperations = jdbcOperations;
        this.baseMapperClass = baseMapperClass;
        this.configuration = configuration;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (Object.class.equals(method.getDeclaringClass())) {
            // Object自带的方法，不走代理调用
            return method.invoke(this, args);
        }

        if (method.isDefault()) {
            // 默认方法执行
            MethodHandle methodHandle = MethodHandlesUtils.getSpecialMethodHandle(method).bindTo(proxy);
            return methodHandle.invokeWithArguments(args);
        }

        AbstractMethodHandler handler = MethodHandlerFactory.create(method);
        handler.setMethod(method);
        handler.setJdbcOperations(jdbcOperations);
        handler.setArgs(args);
        handler.setConfiguration(configuration);

        // 解析出BaseMapper上的泛型
        Type[] classes = ReflectUtils.getGenericClassesForInterface(baseMapperClass);
        if (classes != null) {
            handler.setEntityClass((Class<?>) classes[0]);
            handler.setIdClass((Class<?>) classes[1]);
        }

        return handler.handle();
    }

}
