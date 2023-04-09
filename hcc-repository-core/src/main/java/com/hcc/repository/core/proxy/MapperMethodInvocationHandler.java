package com.hcc.repository.core.proxy;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.MethodHandlerFactory;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.mapper.BaseMapper;
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

    private final JdbcTemplateProxy jdbcTemplateProxy;
    private final Class<?> baseMapperClass;
    private final RepositoryConfiguration configuration;

    public MapperMethodInvocationHandler(JdbcTemplateProxy jdbcTemplateProxy, Class<?> baseMapperClass, RepositoryConfiguration configuration) {
        this.jdbcTemplateProxy = jdbcTemplateProxy;
        this.baseMapperClass = baseMapperClass;
        this.configuration = configuration;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (Object.class.equals(method.getDeclaringClass())
                || !BaseMapper.class.equals(method.getDeclaringClass())) {
            // Object自带的方法，或者不是BaseMapper中定义的方法，不走代理调用
            return method.invoke(this, args);
        }

        String methodName = method.getName();
        if (log.isDebugEnabled()) {
            log.debug("当前执行的方法：{}", methodName);
        }

        if (method.isDefault()) {
            MethodHandle methodHandle = MethodHandlesUtils.getSpecialMethodHandle(method).bindTo(proxy);
            return methodHandle.invokeWithArguments(args);
        }

        AbstractMethodHandler handler = MethodHandlerFactory.create(methodName);
        handler.setMethod(method);
        handler.setJdbcTemplateProxy(jdbcTemplateProxy);
        handler.setArgs(args);
        handler.setConfiguration(configuration);

        // 解析出BaseMapper上的泛型
        Type[] classes = ReflectUtils.getGenericClassesForInterface(baseMapperClass);
        if (classes != null) {
            handler.setEntityClass((Class<?>) classes[0]);
            handler.setIdClass((Class<?>) classes[1]);
        }

        long start = System.currentTimeMillis();
        Object returnVal = handler.handle();
        long end = System.currentTimeMillis();

        if (log.isDebugEnabled()) {
            log.debug("方法：{}执行耗时：{}ms", methodName, end - start);
        }

        return returnVal;
    }

}
