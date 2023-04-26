package com.hcc.repository.test.dao.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.extension.interceptor.ExtInterceptor;
import lombok.extern.slf4j.Slf4j;

/**
 * TestInterceptor
 *
 * @author hushengjun
 * @date 2023/4/7
 */
@Slf4j
public class TestInterceptor implements ExtInterceptor {

    @Override
    public void beforePrepareCondition(MethodNameEnum methodNameEnum, Object[] args) {
    }

    @Override
    public void afterPrepareCondition(MethodNameEnum methodNameEnum, Object[] args, ICondition<?> condition) {
    }

    @Override
    public void beforeExecute(JdbcOperations jdbcOperations, SqlExecuteContext context) {
    }

    @Override
    public void beforeExecuteUpdate(JdbcOperations jdbcOperations, SqlExecuteContext context) {
    }

    @Override
    public void beforeExecuteQuery(JdbcOperations jdbcOperations, SqlExecuteContext context) {
    }

}
