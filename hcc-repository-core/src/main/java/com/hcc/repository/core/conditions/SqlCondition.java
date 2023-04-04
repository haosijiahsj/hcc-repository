package com.hcc.repository.core.conditions;

import com.hcc.repository.core.utils.Assert;

import java.util.HashMap;
import java.util.Map;

/**
 * 用于拼接原生sql
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public class SqlCondition<T> extends ICondition<T> {

    private String sql;
    private final Map<String, Object> paramMap;

    public SqlCondition() {
        paramMap = new HashMap<>(16);
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

    /**
     * sql语句
     * @param namedSql
     * @return
     */
    public SqlCondition<T> sql(String namedSql) {
        Assert.isNotNull(namedSql, "sql不能为空");
        this.sql = namedSql;
        return this;
    }

    /**
     * 添加参数
     * @param name
     * @param val
     * @return
     */
    public SqlCondition<T> putParam(String name, Object val) {
        this.paramMap.put(name, val);
        return this;
    }

    /**
     * 添加多个参数
     * @param paramMap
     * @return
     */
    public SqlCondition<T> putParamMap(Map<String, Object> paramMap) {
        this.paramMap.putAll(paramMap);
        return this;
    }

    /**
     * 重置
     * @return
     */
    public SqlCondition<T> reset() {
        this.sql = null;
        this.paramMap.clear();
        return this;
    }

}
