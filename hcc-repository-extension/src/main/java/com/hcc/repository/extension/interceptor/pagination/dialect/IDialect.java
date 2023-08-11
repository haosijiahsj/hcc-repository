package com.hcc.repository.extension.interceptor.pagination.dialect;

import com.hcc.repository.extension.interceptor.pagination.PaginationContext;

/**
 * 分页语句方言
 *
 * @author hushengjun
 * @date 2023/4/29
 */
public interface IDialect {

    /**
     * 分页处理，在上下文中修改原始sql、原始sql参数并设置count sql
     * @param context
     */
    void handle(PaginationContext context);

}
