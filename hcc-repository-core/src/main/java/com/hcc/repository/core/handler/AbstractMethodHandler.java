package com.hcc.repository.core.handler;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.OriginalSqlCondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.SqlParseUtils;
import lombok.extern.slf4j.Slf4j;

/**
 * 抽象的方法处理器
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public abstract class AbstractMethodHandler {

    protected JdbcTemplateProxy jdbcTemplateProxy;
    private String methodName;
    private MethodNameEnum methodNameEnum;
    protected Class<?> idClass;
    protected Class<?> entityClass;
    protected Object[] args;

    /**
     * 执行入口
     * @return
     * @throws Exception
     */
    public Object handle() throws Exception {
        // 1. prepare
        this.prepare();

        // 2. 构建条件
        ICondition<?> condition = this.assembleCondition();
        if (condition == null) {
            throw new NullPointerException("condition不能为空");
        }
        condition.setEntityClass(entityClass);

        Pair<String, Object[]> pair;
        if (condition instanceof OriginalSqlCondition && !((OriginalSqlCondition<?>) condition).maybeNamedSql()) {
            // 如果是原生sql的方式传参
            pair = Pair.of(condition.getExecuteSql(), ((OriginalSqlCondition<?>) condition).getArgs());
        } else {
            // 真实带有占位符的sql和参数数组
            pair = SqlParseUtils.parseNamedSql(condition.getExecuteSql(),
                    condition.getColumnValuePairs());
        }

        String sqlToUse = pair.getLeft();
        Object[] params = pair.getRight();

        // 3. 执行sql
        return this.executeSql(sqlToUse, params);
    }

    protected void prepare() {
        if (firstArgIsNull()) {
            throw new IllegalArgumentException("参数不能为空！");
        }
    }

    /**
     * 构建查询条件
     * @return
     */
    protected abstract ICondition<?> assembleCondition();

    /**
     * 执行sql
     * @param sql
     * @param args
     * @return
     */
    protected abstract Object executeSql(String sql, Object[] args);

    public void setJdbcTemplateProxy(JdbcTemplateProxy jdbcTemplateProxy) {
        this.jdbcTemplateProxy = jdbcTemplateProxy;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
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
