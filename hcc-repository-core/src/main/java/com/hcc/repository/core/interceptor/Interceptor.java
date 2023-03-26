package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.jdbc.JdbcTemplateProxy;

/**
 * Interceptor
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface Interceptor {

    /**
     * 能否更新
     * @return
     */
    default boolean canUpdate(JdbcTemplateProxy jdbcTemplateWrapper, SqlExecuteContext context) {
        return true;
    }

    /**
     * 能否查询
     * @return
     */
    default boolean canQuery(JdbcTemplateProxy jdbcTemplateWrapper, SqlExecuteContext context) {
        return true;
    }

    /**
     * 更新之前调用
     * @param jdbcTemplateWrapper
     * @param context
     */
    default void beforeUpdate(JdbcTemplateProxy jdbcTemplateWrapper, SqlExecuteContext context) {}

    /**
     * 查询之前调用
     * @param jdbcTemplateProxy
     * @param context
     */
    default void beforeQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {}

}
