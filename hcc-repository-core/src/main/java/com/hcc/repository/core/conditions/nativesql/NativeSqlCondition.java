package com.hcc.repository.core.conditions.nativesql;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 用于拼接原生sql，自行保证sql的正确性<br/>
 * 注意使用named方式和原生数组方式只能选其一
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public class NativeSqlCondition<T> extends ICondition<T> {

    private String sql;
    private Map<String, Object> paramMap;
    private List<Object> args;

    public NativeSqlCondition() {
        paramMap = new HashMap<>(16);
        args = new ArrayList<>(16);
    }

    @Override
    public T getEntity() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Class<?> getEntityClass() {
        return null;
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
    public NativeSqlCondition<T> sql(String sql) {
        Assert.isNotNull(sql, "sql不能为空");
        this.sql = sql;
        return this;
    }

    private void checkParamMap() {
        Assert.isTrue(CollUtils.isEmpty(args), "添加paramMap参数时，args已存在值");
    }

    private void checkArgs() {
        Assert.isTrue(CollUtils.isEmpty(paramMap), "添加args参数时，paramMap已存在值");
    }

    /**
     * 添加参数
     * @param name
     * @param val
     * @return
     */
    public NativeSqlCondition<T> putParam(String name, Object val) {
        this.checkParamMap();
        this.paramMap.put(name, val);
        return this;
    }

    /**
     * 添加多个参数
     * @param paramMap
     * @return
     */
    public NativeSqlCondition<T> putParamMap(Map<String, Object> paramMap) {
        this.checkParamMap();
        this.paramMap.putAll(paramMap);
        return this;
    }

    /**
     * 添加参数
     * @param val
     * @return
     */
    public NativeSqlCondition<T> addArg(Object val) {
        this.checkArgs();
        args.add(val);
        return this;
    }

    /**
     * 添加多个参数
     * @param vals
     * @return
     */
    public NativeSqlCondition<T> addArg(Object... vals) {
        this.checkArgs();
        args.addAll(Arrays.asList(vals));
        return this;
    }

    /**
     * 添加多个参数
     * @param args
     * @return
     */
    public NativeSqlCondition<T> addArgs(List<Object> args) {
        this.checkArgs();
        this.args.addAll(args);
        return this;
    }

    /**
     * 重置
     * @return
     */
    @Override
    public void reset() {
        this.sql = null;
        this.paramMap = new HashMap<>(16);
        this.args = new ArrayList<>(16);
    }

}
