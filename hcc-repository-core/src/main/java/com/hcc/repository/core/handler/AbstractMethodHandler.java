package com.hcc.repository.core.handler;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.ExecuteContext;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.SqlParseUtils;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 抽象的方法处理器
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Slf4j
public abstract class AbstractMethodHandler {

    protected JdbcTemplateWrapper jdbcTemplateWrapper;
    private String methodName;
    private MethodNameEnum methodNameEnum;
    protected Class<?> idClass;
    protected Class<?> entityClass;
    protected Object[] args;
    protected List<Interceptor> interceptors = new ArrayList<>();

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

        // 真实带有占位符的sql和参数数组
        Pair<String, Object[]> pair = SqlParseUtils.parseNamedSql(this.getTargetNamedSql(condition),
                condition.getColumnValuePairs());

        // 封装执行上下文
        ExecuteContext context = new ExecuteContext();
        context.setSql(pair.getLeft());
        context.setArgs(pair.getRight());
        context.setMethodName(methodNameEnum);
        context.setSqlType(methodNameEnum.getSqlType());

        for (Interceptor interceptor : interceptors) {
            if (MethodNameEnum.isR(methodNameEnum)) {
                boolean canQuery = interceptor.canQuery(jdbcTemplateWrapper, context);
                if (!canQuery) {
                    return defaultValueForQuery();
                }
            } else {
                boolean canUpdate = interceptor.canUpdate(jdbcTemplateWrapper, context);
                if (!canUpdate) {
                    return defaultValueForUpdate();
                }
            }
        }

        interceptors.forEach(interceptor -> {
            if (MethodNameEnum.isR(methodNameEnum)) {
                interceptor.beforeQuery(jdbcTemplateWrapper, context);
            } else {
                interceptor.beforeUpdate(jdbcTemplateWrapper, context);
            }
        });

        String sqlToUse = context.getSql();
        Object[] params = pair.getRight();

        if (log.isDebugEnabled()) {
            log.debug("==>  Preparing:  {}", sqlToUse);
            String paramStr = Arrays.stream(params)
                    .map(param -> param == null ? "null" : param + "(" + param.getClass().getSimpleName() + ")")
                    .collect(Collectors.joining(", "));
            log.debug("==>  Parameters: {}", paramStr);
        }

        // 3. 执行sql
        Object result = this.executeSql(sqlToUse, params);

        // 打印结果
        if (log.isDebugEnabled()) {
            String logMsg = "<==       Total: {}";
            int total = 0;
            if (result != null) {
                if (result instanceof Collection) {
                    Collection<?> coll = (Collection<?>) result;
                    total = coll.size();
                    coll.forEach(o -> log.debug("<==         Row: {}", o));
                } else {
                    total = 1;
                    log.debug("<==         Row: {}", result);
                }
            }
            log.debug(logMsg, total);
        }

        return result;
    }

    protected void prepare() {
        if (firstArgIsNull()) {
            throw new IllegalArgumentException("参数不能为空！");
        }
    }

    /**
     * 获取目标sql
     * @param condition
     * @return
     */
    private String getTargetNamedSql(ICondition<?> condition) {
        String namedSql;
        if (MethodNameEnum.isC(methodNameEnum)) {
            namedSql = condition.getSqlInsert();
        } else if (MethodNameEnum.isR(methodNameEnum)) {
            if (MethodNameEnum.SELECT_COUNT.equals(methodNameEnum)) {
                namedSql = condition.getSqlCount();
            } else {
                namedSql = condition.getSqlQuery();
            }
        } else if (MethodNameEnum.isU(methodNameEnum)) {
            namedSql = condition.getSqlUpdate();
        } else if (MethodNameEnum.isD(methodNameEnum)) {
            namedSql = condition.getSqlDelete();
        } else {
            throw new UnsupportedOperationException();
        }

        return namedSql.trim();
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

    /**
     * 更新语句默认值
     * @return
     */
    protected Object defaultValueForUpdate() {
        return -1;
    }

    /**
     * 查询语句默认值
     * @return
     */
    protected Object defaultValueForQuery() {
        return Collections.emptyList();
    }

    public void setJdbcTemplateWrapper(JdbcTemplateWrapper jdbcTemplateWrapper) {
        this.jdbcTemplateWrapper = jdbcTemplateWrapper;
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

    public void setInterceptors(List<Interceptor> interceptors) {
        if (CollUtils.isNotEmpty(interceptors)) {
            this.interceptors = interceptors;
        }
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
