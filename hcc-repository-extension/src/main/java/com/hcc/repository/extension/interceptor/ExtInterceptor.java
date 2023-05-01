package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;

import java.lang.reflect.Method;

/**
 * ExtInterceptor
 *
 * @author hushengjun
 * @date 2023/4/9
 */
public interface ExtInterceptor extends Interceptor {

    @Override
    default void beforeExecute(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
        if (SqlTypeEnum.SELECT.equals(context.getSqlType())) {
            this.beforeExecuteQuery(method, parameters, jdbcOperations, context);
        } else {
            this.beforeExecuteUpdate(method, parameters, jdbcOperations, context);
        }
    }

    /**
     * 更新之前调用
     * @param method
     * @param parameters
     * @param jdbcOperations
     * @param context
     */
    default void beforeExecuteUpdate(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {}

    /**
     * 查询之前调用
     * @param method
     * @param parameters
     * @param jdbcOperations
     * @param context
     */
    default void beforeExecuteQuery(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {}


}
