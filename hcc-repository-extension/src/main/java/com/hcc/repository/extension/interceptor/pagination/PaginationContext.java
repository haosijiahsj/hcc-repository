package com.hcc.repository.extension.interceptor.pagination;

import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.ArrayUtils;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import lombok.Data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 分页上下文
 *
 * @author hushengjun
 * @date 2023/4/29
 */
@Data
public class PaginationContext {
    /**
     * 分页参数
     */
    private IPage<?> pageParam;
    /**
     * 原始sql
     */
    private String originalSql;
    /**
     * 原始查询参数
     */
    private Object[] originalSqlParameters;
    /**
     * 总数sql,方言处理器设置
     */
    private String countSql;
    /**
     * 分页sql,方言处理器设置
     */
    private String pageSql;
    /**
     * 分页查询参数，方言处理器设置
     */
    private List<Object> pageSqlParameters;

    /**
     * 添加分页参数
     * @param arg
     */
    public void addPageSqlParameter(Object arg) {
        Assert.isNotNull(arg, "添加的分页参数不能为空");

        if (CollUtils.isEmpty(pageSqlParameters)) {
            pageSqlParameters = new ArrayList<>();
        }
        pageSqlParameters.add(arg);
    }

    /**
     * 添加分页参数
     * @param args
     */
    public void addPageSqlParameter(Object...args) {
        Assert.isNotNull(args, "添加的分页参数不能为空");

        for (Object arg : args) {
            this.addPageSqlParameter(arg);
        }
    }

    /**
     * 获取分页参数
     * @return
     */
    public Object[] getPageSqlParameters() {
        List<Object> finalPageSqlParameters = new ArrayList<>();

        // 原始参数要放到前面
        if (ArrayUtils.isNotEmpty(originalSqlParameters)) {
            finalPageSqlParameters.addAll(Arrays.asList(originalSqlParameters));
        }

        if (CollUtils.isNotEmpty(pageSqlParameters)) {
            finalPageSqlParameters.addAll(pageSqlParameters);
        }

        return finalPageSqlParameters.toArray();
    }

}
