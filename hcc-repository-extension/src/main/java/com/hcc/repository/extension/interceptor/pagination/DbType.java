package com.hcc.repository.extension.interceptor.pagination;

import com.hcc.repository.extension.interceptor.pagination.dialect.MysqlDialect;
import com.hcc.repository.extension.interceptor.pagination.dialect.IDialect;

/**
 * DbType
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public enum DbType {

    MYSQL(new MysqlDialect());

    private final IDialect dialectHandler;

    DbType(IDialect dialectHandler) {
        this.dialectHandler = dialectHandler;
    }

    public IDialect getDialectHandler() {
        return dialectHandler;
    }
}
