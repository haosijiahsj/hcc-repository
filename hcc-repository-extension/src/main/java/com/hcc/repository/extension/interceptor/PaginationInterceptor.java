package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.extension.interceptor.page.DbType;

/**
 * PageInterceptor
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class PaginationInterceptor implements ExtInterceptor {

    private final DbType dbType;

    public PaginationInterceptor(DbType dbType) {
        this.dbType = dbType;
    }

    @Override
    public void beforeExecuteQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        // 执行count语句
        String originalSql = context.getSql();

        // 改写sql
//        jdbcTemplateProxy.namedQueryForObject()

        // 将总数放入threadLocal中
    }

}
