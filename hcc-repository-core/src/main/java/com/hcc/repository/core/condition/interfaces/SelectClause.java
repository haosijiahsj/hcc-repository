package com.hcc.repository.core.condition.interfaces;

/**
 * Select
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface SelectClause<C, T, R> {

    C select(R...columns);

}
