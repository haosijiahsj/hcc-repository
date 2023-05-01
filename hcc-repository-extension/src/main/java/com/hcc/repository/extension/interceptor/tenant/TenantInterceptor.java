package com.hcc.repository.extension.interceptor.tenant;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;

import java.lang.reflect.Method;

/**
 * 多租户拦截器
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public class TenantInterceptor implements Interceptor {

    private final TenantHandler tenantHandler;

    public TenantInterceptor(TenantHandler tenantHandler) {
        this.tenantHandler = tenantHandler;
    }

    @Override
    public void beforeExecute(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
        // TODO
    }

}
