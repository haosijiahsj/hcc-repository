package com.hcc.repository.extension.interceptor.page;

import com.hcc.repository.core.page.IPage;
import lombok.Data;

/**
 * PaginationContext
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
     * 原始查询产数
     */
    private Object[] originalSqlParameters;
    /**
     * 总数sql
     */
    private String countSql;
    /**
     * 分页sql
     */
    private String pageSql;

    /**
     * 分页查询参数
     */
    private Object[] pageSqlParameters;
}
