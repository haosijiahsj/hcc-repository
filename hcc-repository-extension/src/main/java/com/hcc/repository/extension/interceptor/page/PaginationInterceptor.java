package com.hcc.repository.extension.interceptor.page;

import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.extension.interceptor.ExtInterceptor;
import com.hcc.repository.extension.interceptor.page.dialect.PaginationDialect;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * 分页拦截器
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class PaginationInterceptor implements ExtInterceptor {

    private static final ThreadLocal<IPage<?>> HOLDER = new ThreadLocal<>();

    private DbType dbType;
    private boolean cacheTotal = false;
    private PaginationDialect paginationDialect;

    public PaginationInterceptor(DbType dbType) {
        this.dbType = dbType;
        this.paginationDialect = dbType.getDialectHandler();
    }

    public PaginationInterceptor(DbType dbType, boolean cacheTotal) {
        this.dbType = dbType;
        this.cacheTotal = cacheTotal;
        this.paginationDialect = dbType.getDialectHandler();
    }

    public PaginationInterceptor(PaginationDialect paginationDialect) {
        this.paginationDialect = paginationDialect;
    }

    public PaginationInterceptor(PaginationDialect paginationDialect, boolean cacheTotal) {
        this.paginationDialect = paginationDialect;
        this.cacheTotal = cacheTotal;
    }

    @Override
    public void beforeExecuteQuery(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
        IPage<?> pageParam = null;
        for (Object parameter : parameters) {
            if (parameter instanceof IPage) {
                pageParam = (IPage<?>) parameter;
            }
        }
        if (pageParam == null) {
            return;
        }

        // 分页上下文
        PaginationContext paginationContext = new PaginationContext();
        paginationContext.setPageParam(pageParam);
        paginationContext.setOriginalSql(context.getSql());
        paginationContext.setOriginalSqlParameters(context.getSqlParameters());

        if (paginationDialect == null) {
            throw new IllegalArgumentException("没有方言处理器");
        }

        // 执行sql调整
        paginationDialect.handler(paginationContext);

        if (!IPage.class.isAssignableFrom(method.getReturnType())) {
            // 返回结果不是分页结果，无需执行count语句
            context.setSql(paginationContext.getPageSql());
            context.setSqlParameters(paginationContext.getPageSqlParameters());
            return;
        }

        // 查询总数
        String countSql = paginationContext.getCountSql();
        long total = Optional.ofNullable(jdbcOperations.queryForObject(countSql, context.getSqlParameters(), Long.class))
                .orElse(0L);

        // 分页结果
        IPage<?> pageResult;
        if (IPage.class.equals(method.getReturnType())) {
            // 返回的是IPage接口，使用DefaultPage实例化
            pageResult = new DefaultPage<>();
        } else {
            // 否则使用自定义的Page实例化
            pageResult = (IPage<?>) ReflectUtils.newInstance(method.getReturnType());
        }

        pageResult.setCurPage(pageParam.getCurPage());
        pageResult.setPageSize(pageParam.getPageSize());
        pageResult.setTotalRows(total);
        if (total == 0L) {
            context.setAbortExecute(true);
            pageResult.setRecords(Collections.emptyList());
            context.setReturnValueSupplier(() -> pageResult);
            return;
        }

        // 修改当前执行的sql语句为分页sql语句
        context.setSql(paginationContext.getPageSql());
        context.setSqlParameters(paginationContext.getPageSqlParameters());

        HOLDER.set(pageResult);
    }

    @Override
    public Object beforeReturn(Method method, Object[] parameters, SqlExecuteContext context, Object result) {
        if (!IPage.class.isAssignableFrom(method.getReturnType())) {
            return result;
        }
        IPage<?> pageResult = HOLDER.get();
        if (pageResult == null) {
            return result;
        }
        pageResult.setRecords((List) result);

        return pageResult;
    }

}
