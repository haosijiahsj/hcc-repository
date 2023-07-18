package com.hcc.repository.extension.interceptor.pagination;

import com.hcc.repository.core.constants.DbType;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.JdbcUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import com.hcc.repository.extension.interceptor.ExtInterceptor;
import com.hcc.repository.extension.interceptor.pagination.dialect.DialectFactory;
import com.hcc.repository.extension.interceptor.pagination.dialect.IDialect;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * 分页拦截器
 *
 * @author hushengjun
 * @date 2023/4/8
 */
@Slf4j
public class PaginationInterceptor implements ExtInterceptor {

    private IDialect iDialect;
    private String url;

    public PaginationInterceptor() {}

    public PaginationInterceptor(DbType dbType) {
        this.iDialect = DialectFactory.getDialect(dbType);
    }

    public PaginationInterceptor(IDialect iDialect) {
        this.iDialect = iDialect;
    }

    @Override
    public void beforeExecuteQuery(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {
        IPage<?> pageParam = this.findPageParam(parameters);
        if (pageParam == null) {
            if (log.isDebugEnabled()) {
                log.debug("方法：{}不是分页方法，无需分页拦截器处理", method.getName());
            }
            return;
        }
        if (iDialect == null) {
            iDialect = this.deductDialect(jdbcOperations);
        }
        Assert.isNotNull(iDialect, "没有方言处理器");

        // 分页上下文
        PaginationContext paginationContext = new PaginationContext();
        paginationContext.setPageParam(pageParam);
        paginationContext.setOriginalSql(context.getSql());
        paginationContext.setOriginalSqlParameters(context.getSqlParameters());
        paginationContext.setNeedCount(IPage.class.isAssignableFrom(method.getReturnType()));

        // 执行sql调整
        iDialect.handle(paginationContext);

        if (!paginationContext.isNeedCount()) {
            // 返回结果不是分页结果，无需执行count语句
            context.setSql(paginationContext.getPageSql());
            context.setSqlParameters(paginationContext.getPageSqlParameters());
            return;
        }

        // 查询总数
        // 分页参数如果传了totalRows，则不进行总数查询
        long total = pageParam.getTotalRows();
        if (total <= 0L) {
            total = this.executeCountSql(jdbcOperations, paginationContext);
        }

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
            pageResult.setRecords(Collections.emptyList());
            // 为0设置中断执行标记，后续不再执行返回ReturnValueSupplier获取的结果
            context.setAbortExecute(true);
            context.setReturnValueSupplier(() -> pageResult);
            return;
        }

        // 修改当前执行的sql语句为分页sql语句
        context.setSql(paginationContext.getPageSql());
        context.setSqlParameters(paginationContext.getPageSqlParameters());
        context.setReturnValueSupplier(() -> pageResult);
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

    /**
     * 推断方言处理器
     * @param jdbcOperations
     * @return
     */
    private IDialect deductDialect(JdbcOperations jdbcOperations) {
        try {
            if (StrUtils.isEmpty(url)) {
                url = jdbcOperations.getDataSource().getConnection().getMetaData().getURL();
            }
            return DialectFactory.getDialect(JdbcUtils.getDbType(url));
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 执行count语句
     * @param jdbcOperations
     * @param context
     * @return
     */
    private long executeCountSql(JdbcOperations jdbcOperations, PaginationContext context) {
        return Optional.ofNullable(jdbcOperations.queryForObject(context.getCountSql(), context.getOriginalSqlParameters(), Long.class))
                .orElse(0L);
    }

    @Override
    public Object beforeReturn(Method method, Object[] parameters, SqlExecuteContext context, Object result) {
        if (!IPage.class.isAssignableFrom(method.getReturnType()) || result instanceof IPage) {
            return result;
        }

        IPage<?> pageResult = (IPage<?>) context.getReturnValueSupplier().get();
        if (pageResult == null) {
            return result;
        }
        pageResult.setRecords((List) result);

        return pageResult;
    }

}
