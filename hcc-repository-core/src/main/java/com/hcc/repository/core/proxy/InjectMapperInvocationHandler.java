package com.hcc.repository.core.proxy;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.MethodHandlerFactory;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

/**
 * InjectMapperInvocationHandler
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public class InjectMapperInvocationHandler implements InvocationHandler {

    private JdbcTemplateWrapper jdbcTemplateWrapper;

    public InjectMapperInvocationHandler(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplateWrapper = new JdbcTemplateWrapper(jdbcTemplate);
    }

    public InjectMapperInvocationHandler(JdbcTemplateWrapper jdbcTemplateWrapper) {
        this.jdbcTemplateWrapper = jdbcTemplateWrapper;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String methodName = method.getName();
        log.info("当前执行的方法：{}", methodName);

        AbstractMethodHandler handler = MethodHandlerFactory.create(methodName);
        handler.setMethodName(methodName);
        handler.setJdbcTemplateWrapper(jdbcTemplateWrapper);
        handler.setArgs(args);

        Object returnVal = handler.handle();

        return returnVal;
    }

}
