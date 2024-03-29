package com.hcc.repository.extension.interceptor.pagination.dialect.handler;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.pagination.PaginationContext;
import com.hcc.repository.extension.interceptor.pagination.dialect.AbstractDialect;

/**
 * mysql分页
 *
 * @author hushengjun
 * @date 2023/4/29
 */
public class MysqlDialect extends AbstractDialect {

    protected void handlePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();
        String originalSql = context.getOriginalSql();
        long offset = pageParam.offset();

        // 设置上下文中的参数
        if (offset == 0L) {
            context.setPageSql(originalSql + " LIMIT ?");
            context.addPageSqlParameter(pageParam.getPageSize());
        } else {
            context.setPageSql(originalSql + " LIMIT ?, ?");
            context.addPageSqlParameter(offset, pageParam.getPageSize());
        }
    }

}
