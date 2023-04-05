package com.hcc.repository.core.conditions;

import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 用于拼接原生sql<br/>
 * 注意使用named方式和原生数组方式只能选其一
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public class OriginalSqlCondition<T> extends ICondition<T> {

    private String sql;
    private final Map<String, Object> paramMap;
    private List<Object> args;

    public OriginalSqlCondition() {
        paramMap = new HashMap<>(16);
        args = new ArrayList<>(16);
    }

    @Override
    public T getEntity() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Class<?> getEntityClass() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getExecuteSql() {
        return sql;
    }

    @Override
    public Map<String, Object> getColumnValuePairs() {
        return paramMap;
    }

    public Object[] getArgs() {
        if (CollUtils.isEmpty(args)) {
            return new Object[] {};
        }
        return args.toArray();
    }

    /**
     * 只要paramMap不为空则为named sql
     * @return
     */
    public boolean maybeNamedSql() {
        return CollUtils.isNotEmpty(paramMap);
    }

    /**
     * sql语句
     * @param sql
     * @return
     */
    public OriginalSqlCondition<T> sql(String sql) {
        Assert.isNotNull(sql, "sql不能为空");
        this.sql = sql;
        return this;
    }

    /**
     * 添加参数
     * @param name
     * @param val
     * @return
     */
    public OriginalSqlCondition<T> putParam(String name, Object val) {
        this.paramMap.put(name, val);
        return this;
    }

    /**
     * 添加多个参数
     * @param paramMap
     * @return
     */
    public OriginalSqlCondition<T> putParamMap(Map<String, Object> paramMap) {
        this.paramMap.putAll(paramMap);
        return this;
    }

    /**
     * 添加参数
     * @param val
     * @return
     */
    public OriginalSqlCondition<T> addArg(Object val) {
        args.add(val);
        return this;
    }

    /**
     * 添加多个参数
     * @param vals
     * @return
     */
    public OriginalSqlCondition<T> addArg(Object... vals) {
        args.addAll(Arrays.asList(vals));
        return this;
    }

    /**
     * 添加多个参数
     * @param args
     * @return
     */
    public OriginalSqlCondition<T> addArgs(List<Object> args) {
        this.args.addAll(args);
        return this;
    }

    /**
     * 重置
     * @return
     */
    public OriginalSqlCondition<T> reset() {
        this.sql = null;
        this.paramMap.clear();
        this.args = new ArrayList<>(16);
        return this;
    }

}
