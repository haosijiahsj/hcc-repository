package com.hcc.repository.extension.interceptor.pagination.dialect;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.pagination.PaginationContext;

/**
 * mysql分页
 *
 * @author hushengjun
 * @date 2023/4/29
 */
public class MysqlDialect extends AbstractDialect {

    private static final String LIMIT = " LIMIT ?, ?";

    /**
     * 生成分页sql
     * @param context
     */
    public void handlePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();
        Object[] sqlParameters = context.getOriginalSqlParameters();

        // 设置上下文中的参数
        context.setPageSql(context.getOriginalSql() + LIMIT);
        context.setPageSqlParameters(this.generatePageSqlParameter(sqlParameters, pageParam.offset(), pageParam.getPageSize()));
    }

}
