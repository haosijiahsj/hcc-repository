package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;

/**
 * ExtInterceptor
 *
 * @author hushengjun
 * @date 2023/4/9
 */
public interface ExtInterceptor extends Interceptor {

    @Override
    default void beforeExecute(JdbcOperations jdbcOperations, SqlExecuteContext context) {
        if (SqlTypeEnum.SELECT.equals(context.getSqlType())) {
            this.beforeExecuteQuery(jdbcOperations, context);
        } else {
            this.beforeExecuteUpdate(jdbcOperations, context);
        }
    }

    /**
     * 更新之前调用
     * @param jdbcOperations
     * @param context
     */
    default void beforeExecuteUpdate(JdbcOperations jdbcOperations, SqlExecuteContext context) {}

    /**
     * 查询之前调用
     * @param jdbcOperations
     * @param context
     */
    default void beforeExecuteQuery(JdbcOperations jdbcOperations, SqlExecuteContext context) {}


}
