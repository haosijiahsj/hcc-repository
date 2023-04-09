package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;

/**
 * ExtInterceptor
 *
 * @author hushengjun
 * @date 2023/4/9
 */
public interface ExtInterceptor extends Interceptor {

    @Override
    default void beforeExecute(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        if (SqlTypeEnum.SELECT.equals(context.getSqlType())) {
            this.beforeExecuteQuery(jdbcTemplateProxy, context);
        } else {
            this.beforeExecuteUpdate(jdbcTemplateProxy, context);
        }
    }

    /**
     * 更新之前调用
     * @param jdbcTemplateProxy
     * @param context
     */
    default void beforeExecuteUpdate(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {}

    /**
     * 查询之前调用
     * @param jdbcTemplateProxy
     * @param context
     */
    default void beforeExecuteQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {}


}
