package com.hcc.repository.core.conditions.interfaces;

/**
 * Update
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface SetClause<C, R> {

    C set(boolean condition, R column, Object val);

    default C set(R column, Object val) {
        return set(true, column, val);
    }

    C setSql(boolean condition, String setSql);

    default C setSql(String setSql) {
        return setSql(true, setSql);
    }

}
