package com.hcc.repository.core.condition.interfaces;

/**
 * OrderByClause
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface OrderByClause<C, R> {

    C orderBy(boolean condition, boolean isAsc, R...columns);

    default C orderBy(boolean isAsc, R...columns) {
        return orderBy(true, isAsc, columns);
    }

    default C orderByAsc(boolean condition, R...columns) {
        return orderBy(condition, true, columns);
    }

    default C orderByAsc(R...columns) {
        return orderByAsc(true, columns);
    }

    default C orderByDesc(boolean condition, R...columns) {
        return orderBy(condition, false, columns);
    }

    default C orderByDesc(R...columns) {
        return orderByDesc(true, columns);
    }

}