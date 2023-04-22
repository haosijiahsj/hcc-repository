package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.extension.interceptor.page.DbType;

/**
 * PageInterceptor
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class PaginationInterceptor implements ExtInterceptor {

    private final DbType dbType;

    public PaginationInterceptor(DbType dbType) {
        this.dbType = dbType;
    }

    @Override
    public void afterPrepareCondition(MethodNameEnum methodNameEnum, Object[] args, ICondition<?> condition) {
        if (MethodNameEnum.SELECT_PAGE.getMethodName().equals(methodNameEnum.getMethodName())) {
            if (condition instanceof AbstractQueryCondition) {
                if (DbType.MYSQL.equals(dbType)) {
                    ((AbstractQueryCondition<?, ?, ?>) condition).last(String.format("LIMIT %s, %s", 1, 10));
                }
            }
        }
    }

}
