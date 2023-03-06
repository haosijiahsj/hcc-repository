package com.hcc.repository.core.proxy;

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

    private final JdbcTemplate jdbcTemplate;

    public InjectMapperInvocationHandler(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        String methodName = method.getName();
        log.info("当前执行的方法：{}", methodName);
        return null;
    }

}
