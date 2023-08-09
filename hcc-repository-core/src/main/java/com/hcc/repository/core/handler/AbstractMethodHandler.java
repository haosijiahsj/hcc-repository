package com.hcc.repository.core.handler;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.nativesql.NativeSqlCondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.convert.EnumNameConverter;
import com.hcc.repository.core.convert.EnumOrdinalConverter;
import com.hcc.repository.core.convert.IEnumConverter;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ConstructorUtils;
import com.hcc.repository.core.utils.JSqlParserUtils;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.SqlParseUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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
        // 拦截器Condition准备前方法
        interceptors.forEach(interceptor -> {
            interceptor.customizeConfiguration(configuration);
            interceptor.beforePrepareCondition(method, args);
        });

        // prepare
        this.prepare();

        // 构建条件
        ICondition<?> condition = this.prepareCondition();
        Assert.isNotNull(condition, "condition不能为空");
        condition.setEntityClass(entityClass);

        // 拦截器Condition准备后方法
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
            // 注意后续方法，拦截器都不会执行
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

        // 假分页处理
        IPage<?> pageParam = this.findPageParam(args);
        if (pageParam != null && !(result instanceof IPage)) {
            return this.fakePage(pageParam, result);
        }

        return result;
    }

    /**
     * 假分页
     * @param pageParam
     * @param result
     * @return
     */
    private Object fakePage(IPage<?> pageParam, Object result) {
        if (IPage.class.isAssignableFrom(method.getReturnType())) {
            IPage<?> pageResult;
            if (IPage.class.equals(method.getReturnType())) {
                // 返回的是IPage接口，使用DefaultPage实例化
                pageResult = new DefaultPage<>();
            } else {
                // 否则使用自定义的Page实例化
                pageResult = (IPage<?>) ReflectUtils.newInstance(method.getReturnType());
            }
            List<?> records = new ArrayList<>((Collection<?>) result);
            pageResult.setCurPage(pageParam.getCurPage());
            pageResult.setPageSize(pageParam.getPageSize());
            pageResult.setTotalRows(records.size());
            if (CollUtils.isEmpty(records)) {
                pageResult.setRecords(Collections.emptyList());
            } else {
                // 分割list
                List results = new ArrayList();
                int size = 0;
                for (int i = 0; i < records.size(); i++) {
                    if (i >= pageParam.offset() && size < pageParam.getPageSize()) {
                        size++;
                        results.add(records.get(i));
                    }
                }
                pageResult.setRecords(results);
            }

            return pageResult;
        }

        return result;
    }

    /**
     * 获取分页参数
     * @param parameters
     * @return
     */
    private IPage<?> findPageParam(Object[] parameters) {
        return (IPage<?>) Arrays.stream(parameters)
                .filter(p -> p instanceof IPage)
                .findFirst()
                .orElse(null);
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
     * 获取转换器
     * @param columnInfo
     * @return
     */
    protected IConverter getConverter(TableColumnInfo columnInfo) {
        Class<? extends IConverter> converter = null;
        if (columnInfo.needConvert()) {
            converter = columnInfo.getConverter();
        } else if (columnInfo.isIEnum()) {
            converter = IEnumConverter.class;
        } else if (columnInfo.isEnum()) {
            if (String.class.equals(columnInfo.getFieldType())) {
                converter = EnumNameConverter.class;
            } else if (Integer.class.equals(columnInfo.getFieldType()) || int.class.equals(columnInfo.getFieldType())) {
                converter = EnumOrdinalConverter.class;
            }
        }

        if (converter != null) {
            return this.newInstanceConverter(converter, columnInfo);
        }

        return null;
    }

    /**
     * 实例化converter
     * @param converterClass
     * @param columnInfo
     * @return
     */
    private IConverter newInstanceConverter(Class<? extends IConverter> converterClass, TableColumnInfo columnInfo) {
        return Optional.ofNullable(ReflectUtils.matchConstruct(converterClass, Class.class))
                .map(c -> (IConverter) ConstructorUtils.newInstance(c, columnInfo.getFieldType()))
                .orElseGet(() -> ReflectUtils.newInstance(converterClass));
    }

    /**
     * sql解析
     * @param condition
     * @return
     */
    private Pair<String, Object[]> parseSql(ICondition<?> condition) {
        Pair<String, Object[]> pair;
        if (condition instanceof NativeSqlCondition && !((NativeSqlCondition<?>) condition).maybeNamedSql()) {
            // 如果是原生sql的方式传参，无需解析sql和参数
            pair = Pair.of(condition.getExecuteSql(), ((NativeSqlCondition<?>) condition).getArgs());
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

    public Object getArg(int index) {
        if (args == null) {
            throw new IllegalArgumentException("BaseMapper方法参数列表为空");
        }
        if (args.length - 1 < index) {
            throw new IllegalArgumentException("BaseMapper方法参数越界");
        }
        return args[index];
    }

    public <T> T getArg(int index, Class<T> clazz) {
        return clazz.cast(getArg(index));
    }

    public Object getFirstArg() {
        return getArg(0);
    }

    public <T> T getFirstArg(Class<T> clazz) {
        return getArg(0, clazz);
    }

    public boolean firstArgIsNull() {
        return args == null || args[0] == null;
    }

}
