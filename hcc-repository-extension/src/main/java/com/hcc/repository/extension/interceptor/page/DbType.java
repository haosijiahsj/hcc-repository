package com.hcc.repository.extension.interceptor.page;

import com.hcc.repository.extension.interceptor.page.dialect.MysqlPaginationDialect;
import com.hcc.repository.extension.interceptor.page.dialect.PaginationDialect;

/**
 * DbType
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public enum DbType {

    MYSQL(new MysqlPaginationDialect());

    private final PaginationDialect handler;

    DbType(PaginationDialect handler) {
        this.handler = handler;
    }

    public PaginationDialect getDialectHandler() {
        return handler;
    }
}
