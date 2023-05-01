package com.hcc.repository.extension.interceptor.page.dialect;

import com.hcc.repository.extension.interceptor.page.PaginationContext;

/**
 * PageHandler
 *
 * @author hushengjun
 * @date 2023/4/29
 */
public interface PaginationDialect {

    void handler(PaginationContext context);

}
