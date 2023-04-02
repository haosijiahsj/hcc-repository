package com.hcc.repository.core.proxy;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.MethodHandlerFactory;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.utils.MethodHandlesUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;

import java.lang.invoke.MethodHandle;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

/**
 * InjectMapperInvocationHandler
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public class InjectMapperInvocationHandler implements InvocationHandler {

    private JdbcTemplateProxy jdbcTemplateProxy;
    private Class<?> baseMapperClass;

    public InjectMapperInvocationHandler(JdbcTemplateProxy jdbcTemplateProxy, Class<?> baseMapperClass) {
        this.jdbcTemplateProxy = jdbcTemplateProxy;
        this.baseMapperClass = baseMapperClass;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (Object.class.equals(method.getDeclaringClass())
                || !BaseMapper.class.equals(method.getDeclaringClass())) {
            // Object自带的方法，或者不是BaseMapper中定义的方法，不走代理调用
            return method.invoke(this, args);
        }

        if (method.isDefault()) {
            MethodHandle methodHandle = MethodHandlesUtils.getSpecialMethodHandle(method).bindTo(proxy);
            return methodHandle.invokeWithArguments(args);
        }

        String methodName = method.getName();
        log.info("当前执行的方法：{}", methodName);

        AbstractMethodHandler handler = MethodHandlerFactory.create(methodName);
        handler.setMethodName(methodName);
        handler.setJdbcTemplateProxy(jdbcTemplateProxy);
        handler.setArgs(args);

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
