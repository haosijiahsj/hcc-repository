package com.hcc.repository.extension.interceptor.pagination.dialect.handler;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.pagination.PaginationContext;
import com.hcc.repository.extension.interceptor.pagination.dialect.AbstractDialect;

/**
 * Postgre分页
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public class PostgreDialect extends AbstractDialect {

    private static final String LIMIT = " LIMIT ?";
    private static final String OFFSET = " OFFSET ?";

    @Override
    protected void handlePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();
        Object[] sqlParameters = context.getOriginalSqlParameters();

        // 设置上下文中的参数
        context.setPageSql(context.getOriginalSql() + LIMIT + OFFSET);
        context.setPageSqlParameters(this.generatePageSqlParameter(sqlParameters, pageParam.getPageSize(), pageParam.offset()));
    }

}
