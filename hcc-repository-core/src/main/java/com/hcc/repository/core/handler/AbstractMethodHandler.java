package com.hcc.repository.core.handler;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.original.OriginalSqlCondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.SqlParseUtils;
import com.hcc.repository.core.utils.JSqlParserUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * 抽象的方法处理器
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public abstract class AbstractMethodHandler {

    protected JdbcOperations jdbcOperations;
    protected Method method;
    private String methodName;
    private MethodNameEnum methodNameEnum;
    protected Class<?> idClass;
    protected Class<?> entityClass;
    protected Object[] args;
    protected RepositoryConfiguration configuration;

    /**
     * 执行入口
     * @return
     * @throws Exception
     */
    public Object handle() throws Exception {
        List<Interceptor> interceptors = Optional.ofNullable(configuration.getInterceptors())
                .orElse(Collections.emptyList());
        // 拦截器准备前方法
        interceptors.forEach(interceptor -> interceptor.beforePrepareCondition(method, args));

        // prepare
        this.prepare();

        // 构建条件
        ICondition<?> condition = this.prepareCondition();
        Assert.isNotNull(condition, "condition不能为空");
        condition.setEntityClass(entityClass);

        // 拦截器准备后方法
        interceptors.forEach(interceptor -> interceptor.afterPrepareCondition(method, args, condition));

        // 解析sql
        Pair<String, Object[]> pair = this.parseSql(condition);
        String sqlToUse = pair.getLeft();
        Object[] sqlParameters = pair.getRight();

        // sql执行上下文
        SqlExecuteContext context = new SqlExecuteContext();
        context.setSql(sqlToUse);
        context.setSqlType(JSqlParserUtils.getSqlType(context.getSql()));
        context.setSqlParameters(sqlParameters);

        // sql执行前拦截方法
        for (Interceptor interceptor : interceptors) {
            interceptor.beforeExecute(method, args, jdbcOperations, context);
            if (context.isAbortExecute()) {
                return context.getReturnValueSupplier().get();
            }
        }

        // 执行sql
        Object result = this.executeSql(context.getSql(), context.getSqlParameters());

        // sql执行后拦截方法
        for (Interceptor interceptor : interceptors) {
            result = interceptor.afterExecute(method, args, jdbcOperations, context, result);
        }

        // 返回前执行拦截方法
        for (Interceptor interceptor : interceptors) {
            result = interceptor.beforeReturn(method, args, context, result);
        }

        return result;
    }

    protected void prepare() {
        Assert.isFalse(firstArgIsNull(), "第一个参数不能为空！");
    }

    /**
     * 构建查询条件
     * @return
     */
    protected abstract ICondition<?> prepareCondition();

    /**
     * 执行sql
     * @param sql
     * @param args
     * @return
     */
    protected abstract Object executeSql(String sql, Object[] args);

    /**
     * sql解析
     * @param condition
     * @return
     */
    private Pair<String, Object[]> parseSql(ICondition<?> condition) {
        Pair<String, Object[]> pair;
        if (condition instanceof OriginalSqlCondition && !((OriginalSqlCondition<?>) condition).maybeNamedSql()) {
            // 如果是原生sql的方式传参
            pair = Pair.of(condition.getExecuteSql(), ((OriginalSqlCondition<?>) condition).getArgs());
        } else {
            // 真实带有占位符的sql和参数数组
            pair = SqlParseUtils.parsePlaceholderSql(condition.getExecuteSql(), condition.getColumnValuePairs());
        }

        return pair;
    }

    public void setJdbcOperations(JdbcOperations jdbcOperations) {
        this.jdbcOperations = jdbcOperations;
    }

    public void setMethod(Method method) {
        this.method = method;
        this.methodName = method.getName();
        this.methodNameEnum = MethodNameEnum.get(methodName);
    }

    public String getMethodName() {
        return methodName;
    }

    public void setArgs(Object[] args) {
        this.args = args;
    }

    public void setIdClass(Class<?> idClass) {
        this.idClass = idClass;
    }

    public void setEntityClass(Class<?> entityClass) {
        this.entityClass = entityClass;
    }

    public void setConfiguration(RepositoryConfiguration configuration) {
        this.configuration = configuration;
    }

    public Object getFirstArg() {
        return args[0];
    }

    public <T> T getFirstArg(Class<T> clazz) {
        return clazz.cast(args[0]);
    }

    public boolean firstArgIsNull() {
        return args == null || args[0] == null;
    }

}
