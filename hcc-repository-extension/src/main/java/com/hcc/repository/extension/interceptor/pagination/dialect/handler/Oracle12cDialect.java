package com.hcc.repository.extension.interceptor.pagination.dialect.handler;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.pagination.PaginationContext;
import com.hcc.repository.extension.interceptor.pagination.dialect.AbstractDialect;

/**
 * Oracle12c分页
 *
 * @author hushengjun
 * @date 2023/7/12
 */
public class Oracle12cDialect extends AbstractDialect {

    @Override
    protected void handlePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();

        String originalSql = context.getOriginalSql();
        String sql = originalSql + " OFFSET ? ROWS FETCH NEXT ? ROWS ONLY";

        context.setPageSql(sql);
        context.addPageSqlParameter(pageParam.offset(), pageParam.getPageSize());
    }

}
