package com.hcc.repository.core.condition.interfaces;

/**
 * ExtWhereClause
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface ExtWhereClause<C, R> {

    C last(boolean condition, String lastSql);

    default C last(String lastSql) {
        return last(true, lastSql);
    }

    C apply(boolean condition, String sql);

    default C apply(String sql) {
        return apply(true, sql);
    }

    C exists(boolean condition, String sql);

    default C exists(String sql) {
        return exists(true, sql);
    }

    C notExists(boolean condition, String sql);

    default C notExists(String sql) {
        return notExists(true, sql);
    }

}
