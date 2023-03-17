package com.hcc.repository.core.condition.interfaces;

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

    C having(boolean condition, String havingSql, String...params);

    default C having(String havingSql, String...params) {
        return having(true, havingSql, params);
    }

}
