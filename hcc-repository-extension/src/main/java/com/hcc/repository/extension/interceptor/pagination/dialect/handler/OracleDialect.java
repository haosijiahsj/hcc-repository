package com.hcc.repository.extension.interceptor.pagination.dialect.handler;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.pagination.PaginationContext;
import com.hcc.repository.extension.interceptor.pagination.dialect.AbstractDialect;

/**
 * oracle分页
 *
 * @author hushengjun
 * @date 2023/7/12
 */
public class OracleDialect extends AbstractDialect {

    @Override
    protected void handlePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();
        long offset = pageParam.offset();
        long limit = pageParam.getPageSize();

        limit = (offset >= 1) ? (offset + limit) : limit;
        String sql = "SELECT * FROM ( SELECT TMP.*, ROWNUM ROW_ID FROM ( " + context.getOriginalSql() + " ) TMP WHERE ROWNUM <= ? ) WHERE ROW_ID > ?";

        context.setPageSql(sql);
        context.addPageSqlParameter(limit, offset);
    }

}
