package com.hcc.repository.extension.interceptor.pagination.dialect.handler;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.pagination.PaginationContext;
import com.hcc.repository.extension.interceptor.pagination.dialect.AbstractDialect;

/**
 * PostgreSQL分页
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public class PostgreSQLDialect extends AbstractDialect {

    @Override
    protected void handlePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();
        long offset = pageParam.offset();

        // 设置上下文中的参数
        if (offset == 0L) {
            context.setPageSql(context.getOriginalSql() + " LIMIT ?");
            context.addPageSqlParameter(pageParam.getPageSize());
        } else {
            context.setPageSql(context.getOriginalSql() + " LIMIT ? OFFSET ?");
            context.addPageSqlParameter(pageParam.getPageSize(), offset);
        }
    }

}
