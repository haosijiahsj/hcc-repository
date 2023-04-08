package com.hcc.repository.test.dao.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import lombok.extern.slf4j.Slf4j;

/**
 * TestInterceptor
 *
 * @author hushengjun
 * @date 2023/4/7
 */
@Slf4j
public class TestInterceptor implements Interceptor {

    @Override
    public void beforePrepareCondition(MethodNameEnum methodNameEnum, Object[] args) {
        log.info("执行了beforePrepareCondition拦截器");
    }

    @Override
    public void afterPrepareCondition(MethodNameEnum methodNameEnum, ICondition<?> condition) {
        log.info("执行了afterPrepareCondition拦截器");
    }

    @Override
    public void beforeExecute(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        log.info("执行了beforeExecute拦截器");
    }

    @Override
    public void beforeExecuteUpdate(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        log.info("执行了beforeExecuteQuery拦截器");
    }

    @Override
    public void beforeExecuteQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        log.info("执行了beforeExecuteQuery拦截器");
    }

}
