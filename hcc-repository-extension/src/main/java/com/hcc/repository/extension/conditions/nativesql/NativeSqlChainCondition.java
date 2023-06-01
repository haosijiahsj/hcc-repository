package com.hcc.repository.extension.conditions.nativesql;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.nativesql.NativeSqlCondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.extension.conditions.query.ChainQuery;
import com.hcc.repository.extension.conditions.update.ChainUpdate;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * OriginalSqlChainCondition
 *
 * @author hushengjun
 * @date 2023/4/6
 */
public class NativeSqlChainCondition<T, ID extends Serializable> extends ICondition<T> implements ChainUpdate<T, ID>, ChainQuery<T, ID> {

    private final NativeSqlCondition<T> condition;
    private final BaseMapper<T, ID> baseMapper;

    public NativeSqlChainCondition(BaseMapper<T, ID> baseMapper) {
        this.baseMapper = baseMapper;
        this.condition = new NativeSqlCondition<>();
    }

    @Override
    public T getEntity() {
        return condition.getEntity();
    }

    @Override
    public Class<?> getEntityClass() {
        return condition.getEntityClass();
    }

    @Override
    public String getExecuteSql() {
        return condition.getExecuteSql();
    }

    @Override
    public Map<String, Object> getColumnValuePairs() {
        return condition.getColumnValuePairs();
    }

    /**
     * sql语句
     * @param sql
     * @return
     */
    public NativeSqlChainCondition<T, ID> sql(String sql) {
        condition.sql(sql);
        return this;
    }

    /**
     * 添加参数
     * @param name
     * @param val
     * @return
     */
    public NativeSqlChainCondition<T, ID> putParam(String name, Object val) {
        condition.putParam(name, val);
        return this;
    }

    /**
     * 添加多个参数
     * @param paramMap
     * @return
     */
    public NativeSqlChainCondition<T, ID> putParamMap(Map<String, Object> paramMap) {
        condition.putParamMap(paramMap);
        return this;
    }

    /**
     * 添加参数
     * @param val
     * @return
     */
    public NativeSqlChainCondition<T, ID> addArg(Object val) {
        condition.addArg(val);
        return this;
    }

    /**
     * 添加多个参数
     * @param vals
     * @return
     */
    public NativeSqlChainCondition<T, ID> addArg(Object... vals) {
        condition.addArg(vals);
        return this;
    }

    /**
     * 添加多个参数
     * @param args
     * @return
     */
    public NativeSqlChainCondition<T, ID> addArgs(List<Object> args) {
        condition.addArgs(args);
        return this;
    }

    @Override
    public BaseMapper<T, ID> getBaseMapper() {
        return baseMapper;
    }

    @Override
    public ICondition<T> getCondition() {
        return condition;
    }

}
