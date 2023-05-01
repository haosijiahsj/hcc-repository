package com.hcc.repository.extension.interceptor.page.dialect;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.interceptor.page.PaginationContext;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MysqlPageHandler
 *
 * @author hushengjun
 * @date 2023/4/29
 */
public class MysqlPaginationDialect implements PaginationDialect {

    private static final String LIMIT = " LIMIT ";

    private static final Pattern SELECT_PATTERN = Pattern.compile("^\\s*SELECT\\s+DISTINCT\\s+(\\S+)\\s+FROM\\s+.+", Pattern.CASE_INSENSITIVE);
    private static final Pattern FROM_PATTERN = Pattern.compile("^.*\\s*(FROM)\\s+.+", Pattern.CASE_INSENSITIVE);
    private static final Pattern ORDER_BY_PATTERN = Pattern.compile("\\s+(ORDER\\s+BY)\\s+[^'\"]", Pattern.CASE_INSENSITIVE);
    private static final Pattern GROUP_BY_PATTERN = Pattern.compile("\\s+(GROUP\\s+BY)\\s+[^'\"]", Pattern.CASE_INSENSITIVE);


    @Override
    public void handler(PaginationContext context) {
        this.generateCountSql(context);
        this.generatePageSql(context);
    }

    /**
     * 生成分页sql
     * @param context
     */
    public void generatePageSql(PaginationContext context) {
        IPage<?> pageParam = context.getPageParam();
        Object[] sqlParameters = context.getOriginalSqlParameters();
        long offset = pageParam.offset();

        String pageSql = context.getOriginalSql() + LIMIT + "?, ?";

        Object[] pageSqlParameters = new Object[sqlParameters.length + 2];
        // 原参数列表大于0才进行复制
        if (sqlParameters.length > 0) {
            System.arraycopy(sqlParameters, 0, pageSqlParameters, 0, sqlParameters.length);
        }
        pageSqlParameters[pageSqlParameters.length - 2] = offset;
        pageSqlParameters[pageSqlParameters.length - 1] = pageParam.getPageSize();

        context.setPageSql(pageSql);
        context.setPageSqlParameters(pageSqlParameters);
    }

    public void generateCountSql(PaginationContext context) {
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

        // 若sql中含有GROUP BY，则还需要在外层包装一个COUNT(1)
        Matcher groupByMatcher = GROUP_BY_PATTERN.matcher(originalSql);
        originalSql = countSql + originalSql;
        if (groupByMatcher.find()) {
            originalSql = "SELECT COUNT(*) FROM (" + originalSql + ") AS tmp";
        }

        context.setCountSql(originalSql);
    }

}
