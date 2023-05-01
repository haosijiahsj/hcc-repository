package com.hcc.repository.test.dao.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.extension.interceptor.ExtInterceptor;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;

/**
 * TestInterceptor
 *
 * @author hushengjun
 * @date 2023/4/7
 */
@Slf4j
public class TestInterceptor implements ExtInterceptor {

    @Override
    public void beforePrepareCondition(Method method, Object[] parameters) {
    }

    @Override
    public void afterPrepareCondition(Method method, Object[] parameters, ICondition<?> condition) {
    }

    @Override
    public void beforeExecuteUpdate(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
    }

    @Override
    public void beforeExecuteQuery(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
    }

}
