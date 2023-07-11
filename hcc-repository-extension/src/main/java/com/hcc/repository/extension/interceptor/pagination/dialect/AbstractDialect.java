package com.hcc.repository.extension.interceptor.pagination.dialect;

import com.hcc.repository.extension.interceptor.pagination.PaginationContext;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 抽象分页处理器
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public abstract class AbstractDialect implements IDialect {

    private static final Pattern SELECT_PATTERN = Pattern.compile("^\\s*SELECT\\s+DISTINCT\\s+(\\S+)\\s+FROM\\s+.+", Pattern.CASE_INSENSITIVE);
    private static final Pattern FROM_PATTERN = Pattern.compile("^.*\\s*(FROM)\\s+.+", Pattern.CASE_INSENSITIVE);
    private static final Pattern ORDER_BY_PATTERN = Pattern.compile("\\s+(ORDER\\s+BY)\\s+[^'\"]", Pattern.CASE_INSENSITIVE);
    private static final Pattern GROUP_BY_PATTERN = Pattern.compile("\\s+(GROUP\\s+BY)\\s+[^'\"]", Pattern.CASE_INSENSITIVE);

    @Override
    public void handle(PaginationContext context) {
        this.handlePageSql(context);
        this.handleCountSql(context);
    }

    /**
     * 分页语句
     * @param context
     */
    protected abstract void handlePageSql(PaginationContext context);

    /**
     * 默认的生成分页sql参数
     * @param sqlParameters
     * @param first
     * @param second
     * @return
     */
    protected Object[] generatePageSqlParameter(Object[] sqlParameters, long first, long second) {
        if (sqlParameters == null) {
            sqlParameters = new Object[] {};
        }
        // 分页参数
        Object[] pageSqlParameters = new Object[sqlParameters.length + 2];
        // 原参数列表大于0才进行复制
        if (sqlParameters.length > 0) {
            System.arraycopy(sqlParameters, 0, pageSqlParameters, 0, sqlParameters.length);
        }
        pageSqlParameters[pageSqlParameters.length - 2] = first;
        pageSqlParameters[pageSqlParameters.length - 1] = second;

        return pageSqlParameters;
    }

//    /**
//     * count语句，先搞个简单的
//     * @param context
//     */
//    protected void handleCountSql(PaginationContext context) {
//        context.setCountSql("SELECT COUNT(*) FROM (" + context.getOriginalSql() + ") TOTAL");
//    }

    /**
     * count语句
     * @param context
     */
    protected void handleCountSql(PaginationContext context) {
        String originalSql = context.getOriginalSql();
        // 拼接头部
        Matcher selectMatcher = SELECT_PATTERN.matcher(originalSql);
        String countSql = selectMatcher.find() ? "SELECT COUNT(" + selectMatcher.group(1) + ") " : "SELECT COUNT(*) ";

        // 去掉FROM前面的字符
        Matcher fromMatcher = FROM_PATTERN.matcher(originalSql);
        int indexOfFrom = fromMatcher.find() ? fromMatcher.start(1) : -1;
        originalSql = originalSql.substring(indexOfFrom);

        // 去掉ORDER BY后面的字符
        Matcher orderByMatcher = ORDER_BY_PATTERN.matcher(originalSql);
        int indexOfOrderBy = orderByMatcher.find() ? orderByMatcher.start(1) : -1;
        originalSql = indexOfOrderBy != -1 ? originalSql.substring(0, indexOfOrderBy) : originalSql;

        // 若sql中含有GROUP BY，则还需要在外层包装一个COUNT(*)
        Matcher groupByMatcher = GROUP_BY_PATTERN.matcher(originalSql);
        originalSql = countSql + originalSql;
        if (groupByMatcher.find()) {
            originalSql = "SELECT COUNT(*) FROM (" + originalSql + ") AS TOTAL";
        }

        context.setCountSql(originalSql);
    }

}
