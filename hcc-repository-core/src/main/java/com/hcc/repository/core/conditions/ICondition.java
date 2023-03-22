package com.hcc.repository.core.conditions;

import com.hcc.repository.core.metadata.TableInfoHelper;

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

    public abstract Map<String, Object> getColumnValuePairs();

    public String getSqlSet() {
        return null;
    }

    public String getSqlSelect() {
        return null;
    }

    public String getSqlWhere() {
        return null;
    }

    /**
     * 获取查询sql
     * @return
     */
    public String getSqlQuery() {
        return getSqlSelect()
                + " FROM " + TableInfoHelper.getTableName(this.getEntityClass()) + " "
                + getSqlWhere();
    }

    /**
     * 获取更新sql
     * @return
     */
    public String getSqlUpdate() {
        return "UPDATE " + TableInfoHelper.getTableName(this.getEntityClass())
                + " " + getSqlSet() + " "
                + getSqlWhere();
    }

    /**
     * 获取删除sql
     * @return
     */
    public String getSqlDelete() {
        return "DELETE FROM " + TableInfoHelper.getTableName(this.getEntityClass())
                + " " + getSqlWhere();
    }

    public String getSqlInsert() {
        throw new UnsupportedOperationException();
    }

}
