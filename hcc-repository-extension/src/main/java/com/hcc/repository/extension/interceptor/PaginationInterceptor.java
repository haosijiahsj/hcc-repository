package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;

/**
 * PageInterceptor
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class PaginationInterceptor implements Interceptor {

    @Override
    public void beforeExecuteQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        // 执行count语句

        // 改写sql

        // 将总数放入threadLocal中
    }

}
