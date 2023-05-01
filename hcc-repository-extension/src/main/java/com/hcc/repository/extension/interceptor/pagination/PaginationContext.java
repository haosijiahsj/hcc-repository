package com.hcc.repository.extension.interceptor.pagination;

import com.hcc.repository.core.page.IPage;
import lombok.Data;

/**
 * 分页上下文
 *
 * @author hushengjun
 * @date 2023/4/29
 */
@Data
public class PaginationContext {
    /**
     * 分页参数
     */
    private IPage<?> pageParam;
    /**
     * 原始sql
     */
    private String originalSql;
    /**
     * 原始查询参数
     */
    private Object[] originalSqlParameters;
    /**
     * 总数sql,方言处理器设置
     */
    private String countSql;
    /**
     * 分页sql,方言处理器设置
     */
    private String pageSql;

    /**
     * 分页查询参数，方言处理器设置
     */
    private Object[] pageSqlParameters;
}
