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

    /**
     * 获取实体类class类型
     * @return
     */
    public abstract Class<?> getEntityClass();

    /**
     * set实体class
     * @param entityClass
     */
    public void setEntityClass(Class<?> entityClass) {}

    /**
     * 设置执行的sql类型，会改变获取sql的类型
     * @param selectSqlType
     */
    public void setExecuteSqlType(ExecuteSqlTypeEnum selectSqlType) {}

    /**
     * 重置
     */
    public void reset() {}

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

    /**
     * 获取where后的sql
     * @return
     */
    public String getSqlAfterWhere() {
        return "";
    }

}
