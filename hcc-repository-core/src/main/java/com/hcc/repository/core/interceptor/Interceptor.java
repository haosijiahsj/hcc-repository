package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;

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
    default boolean canUpdate(JdbcTemplateWrapper jdbcTemplateWrapper, ExecuteContext context) {
        return true;
    }

    /**
     * 能否查询
     * @return
     */
    default boolean canQuery(JdbcTemplateWrapper jdbcTemplateWrapper, ExecuteContext context) {
        return true;
    }

}
