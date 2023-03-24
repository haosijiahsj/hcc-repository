package com.hcc.repository.core.conditions;

import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.StrUtils;

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

    /**
     * update语句set片段
     * @return
     */
    protected String getSqlSet() {
        throw new UnsupportedOperationException();
    }

    /**
     * select语句select片段
     * @return
     */
    protected String getSqlSelect() {
        throw new UnsupportedOperationException();
    }

    /**
     * where条件后的sql片段
     * @return
     */
    public String getSqlWhere() {
        return null;
    }

    /**
     * 获取插入sql
     * @return
     */
    public String getSqlInsert() {
        throw new UnsupportedOperationException();
    }

    /**
     * 获取删除sql
     * @return
     */
    public String getSqlDelete() {
        return "DELETE FROM "
                + TableInfoHelper.getTableName(this.getEntityClass())
                + " "
                + getSqlWhere();
    }

    /**
     * 获取查询sql
     * @return
     */
    public String getSqlQuery() {
        return getSqlSelect()
                + " FROM "
                + TableInfoHelper.getTableName(this.getEntityClass())
                + " "
                + getSqlWhere();
    }

    /**
     * 获取统计sql
     * @return
     */
    public String getSqlCount() {
        return "SELECT COUNT(*) FROM "
                + TableInfoHelper.getTableName(this.getEntityClass())
                + " "
                + getSqlWhere();
    }

    /**
     * 获取更新sql
     * @return
     */
    public String getSqlUpdate() {
        String sqlSet = getSqlSet();
        if (StrUtils.isEmpty(sqlSet)) {
            throw new IllegalArgumentException("没有set的sql片段");
        }
        return "UPDATE "
                + TableInfoHelper.getTableName(this.getEntityClass())
                + " "
                + sqlSet
                + " "
                + getSqlWhere();
    }

}
