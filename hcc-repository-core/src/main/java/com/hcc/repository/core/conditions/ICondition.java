package com.hcc.repository.core.conditions;

import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;

import java.util.Map;

/**
 * Conditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public abstract class ICondition<T> {

    public abstract T getEntity();

    public abstract Class<?> getEntityClass();

    public void setEntityClass(Class entityClass) {}

    public void setExecuteSqlType(ExecuteSqlTypeEnum selectSqlType) {}

    /**
     * 获取执行的sql
     * @return
     */
    public abstract String getExecuteSql();

    /**
     * 获取sql的参数map
     * @return
     */
    public abstract Map<String, Object> getColumnValuePairs();

}
