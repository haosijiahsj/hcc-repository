package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.jdbc.JdbcTemplateProxy;

/**
 * Interceptor1
 *
 * @author hushengjun
 * @date 2023/3/26
 */
public class Interceptor1 implements Interceptor {

    @Override
    public boolean canQuery(JdbcTemplateProxy jdbcTemplateWrapper, SqlExecuteContext context) {
        return true;
    }

    @Override
    public void beforeQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
//        boolean namedSql = context.isNamedSql();
//        context.setSql("select * FROM table_test");
//        context.setArgs(new Object[]{});
    }

}
