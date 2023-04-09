package com.hcc.repository.core.proxy;

import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import lombok.extern.slf4j.Slf4j;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.update.Update;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * JdbcTemplateProxyInvocationHandler
 *
 * @author hushengjun
 * @date 2023/3/26
 */
@Slf4j
public class JdbcTemplateProxyInvocationHandler implements InvocationHandler {

    private List<Interceptor> interceptors = new ArrayList<>();
    private final JdbcTemplateProxy jdbcTemplateProxy;

    public JdbcTemplateProxyInvocationHandler(JdbcTemplateProxy jdbcTemplateProxy, List<Interceptor> interceptors) {
        this.jdbcTemplateProxy = jdbcTemplateProxy;
        if (CollUtils.isNotEmpty(interceptors)) {
            this.interceptors = interceptors;
        }
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        Assert.isTrue(args != null && args[0] != null, "sql参数不能为空");

        String methodName = method.getName();
        Class<?> returnType = method.getReturnType();

        // sql执行上下文
        SqlExecuteContext context = new SqlExecuteContext();
        context.setNamedSql(this.isNamedSql(methodName));
        // 提供的方法，sql是第一个参数
        context.setSql(args[0].toString());
        // sql的参数是第二个参数
        context.setSqlParameter(args[1]);
        context.setSqlType(this.getSqlType(context.getSql()));

        // 拦截器处理逻辑
        for (Interceptor interceptor : interceptors) {
            interceptor.beforeExecute(jdbcTemplateProxy, context);
        }

        // 经过拦截器重新赋值
        args[0] = context.getSql();
        args[1] = context.getSqlParameter();

        // 执行方法
        Object result = method.invoke(jdbcTemplateProxy, args);
        // 执行查询后的拦截器
        for (Interceptor interceptor : interceptors) {
            result = interceptor.beforeReturn(jdbcTemplateProxy, context, result);
        }

        return result;
    }

    /**
     * 是否是具名sql
     * @param methodName
     * @return
     */
    private boolean isNamedSql(String methodName) {
        return methodName.startsWith("named");
    }

    /**
     * 解析判断sql类型
     * @param sql
     * @return
     * @throws JSQLParserException
     */
    private SqlTypeEnum getSqlType(String sql) throws JSQLParserException {
        Statement statement = CCJSqlParserUtil.parse(sql);
        if (statement instanceof Insert) {
            return SqlTypeEnum.INSERT;
        } else if (statement instanceof Delete) {
            return SqlTypeEnum.DELETE;
        } else if (statement instanceof Update) {
            return SqlTypeEnum.UPDATE;
        } else if (statement instanceof Select) {
            return SqlTypeEnum.SELECT;
        }

        throw new IllegalArgumentException(String.format("sql: %s解析失败", sql));
    }

}
