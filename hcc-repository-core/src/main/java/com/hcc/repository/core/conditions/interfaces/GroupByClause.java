package com.hcc.repository.core.conditions.interfaces;

/**
 * GroupByClause
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface GroupByClause<C, R> {

    C groupBy(boolean condition, R...columns);

    default C groupBy(R...columns) {
        return groupBy(true, columns);
    }

    default C groupBy(R column) {
        return groupBy(true, column);
    }

    C having(boolean condition, String havingSql);

    default C having(String havingSql) {
        return having(true, havingSql);
    }

}
