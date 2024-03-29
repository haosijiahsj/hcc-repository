package com.hcc.repository.extension.interceptor.dynamictablename;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.utils.Assert;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * 动态表名拦截器
 *
 * @author hushengjun
 * @date 2023/4/7
 */
@Slf4j
public class DynamicTableNameInterceptor implements Interceptor {

    private final TableNameHandler tableNameHandler;

    public DynamicTableNameInterceptor(TableNameHandler tableNameHandler) {
        this.tableNameHandler = tableNameHandler;
    }

    @Override
    public void beforeExecute(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
        if (log.isDebugEnabled()) {
            log.debug("原始sql: {}", context.getSql());
        }
        String newSql = this.changeTableName(context.getSql());
        if (log.isDebugEnabled()) {
            log.debug("修改表名后的sql: {}", context.getSql());
        }
        context.setSql(newSql);
    }

    /**
     * 修改表名方法
     * @param sql
     * @return
     */
    protected String changeTableName(String sql) {
        Assert.isNotNull(sql, "原sql不能为空");

        TableNameParser parser = new TableNameParser(sql);
        List<TableNameParser.SqlToken> names = new ArrayList<>();
        parser.accept(names::add);
        StringBuilder builder = new StringBuilder();
        int last = 0;
        for (TableNameParser.SqlToken name : names) {
            int start = name.getStart();
            if (start != last) {
                builder.append(sql, last, start);
                builder.append(tableNameHandler.tableName(name.getValue(), sql));
            }
            last = name.getEnd();
        }
        if (last != sql.length()) {
            builder.append(sql.substring(last));
        }
        return builder.toString();
    }

}
