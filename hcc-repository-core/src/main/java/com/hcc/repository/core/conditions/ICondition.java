package com.hcc.repository.core.conditions;

import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;
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
        return StrPool.EMPTY;
    }

    private String tableName() {
        return TableInfoHelper.getTableName(this.getEntityClass());
    }

    private String joinSpace(String...strs) {
        return String.join(StrPool.SPACE, strs);
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
        return joinSpace(
                SqlKeywordEnum.DELETE_FROM.getKeyword(),
                tableName(),
                getSqlWhere()
        );
    }

    /**
     * 获取查询sql
     * @return
     */
    public String getSqlQuery() {
        return joinSpace(
                getSqlSelect(),
                SqlKeywordEnum.FROM.getKeyword(),
                tableName(),
                getSqlWhere()
        );
    }

    /**
     * 获取统计sql
     * @return
     */
    public String getSqlCount() {
        return joinSpace(
                SqlKeywordEnum.SELECT.getKeyword(),
                SqlKeywordEnum.COUNT.getKeyword(),
                SqlKeywordEnum.FROM.getKeyword(),
                tableName(),
                getSqlWhere()
        );
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

        return joinSpace(
                SqlKeywordEnum.UPDATE.getKeyword(),
                tableName(),
                sqlSet,
                getSqlWhere()
        );
    }

}
