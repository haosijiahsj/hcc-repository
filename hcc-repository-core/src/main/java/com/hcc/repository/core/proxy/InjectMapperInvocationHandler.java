package com.hcc.repository.core.proxy;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.MethodHandlerFactory;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.utils.ArrayUtils;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;

/**
 * InjectMapperInvocationHandler
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public class InjectMapperInvocationHandler implements InvocationHandler {

    private JdbcTemplateWrapper jdbcTemplateWrapper;
    private Class<?> baseMapperClass;

    public InjectMapperInvocationHandler(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplateWrapper = new JdbcTemplateWrapper(jdbcTemplate);
    }

    public InjectMapperInvocationHandler(JdbcTemplateWrapper jdbcTemplateWrapper, Class<?> baseMapperClass) {
        this.jdbcTemplateWrapper = jdbcTemplateWrapper;
        this.baseMapperClass = baseMapperClass;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (Object.class.equals(method.getDeclaringClass())
                || !BaseMapper.class.equals(method.getDeclaringClass())) {
            // Object自带的方法，或者不是BaseMapper中定义的方法，不走代理调用
            return method.invoke(this, args);
        }

        String methodName = method.getName();
        log.info("当前执行的方法：{}", methodName);

        AbstractMethodHandler handler = MethodHandlerFactory.create(methodName);
        handler.setMethodName(methodName);
        handler.setJdbcTemplateWrapper(jdbcTemplateWrapper);
        handler.setArgs(args);

        // 解析出BaseMapper上的泛型
        Type[] classes = ReflectUtils.getGenericClassesForInterface(baseMapperClass);
        if (classes != null) {
            handler.setEntityClass((Class<?>) classes[0]);
            handler.setIdClass((Class<?>) classes[1]);
        }

        Object returnVal = handler.handle();

        return returnVal;
    }

}
