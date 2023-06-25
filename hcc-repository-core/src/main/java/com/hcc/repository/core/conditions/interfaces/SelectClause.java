package com.hcc.repository.core.conditions.interfaces;

import com.hcc.repository.core.metadata.TableColumnInfo;

import java.util.function.Predicate;

/**
 * Select
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface SelectClause<C, T, R> {

    C select(R...columns);

    C selectDistinct(R...columns);

    C select(Class<T> entityClass, Predicate<TableColumnInfo> predicate);

    default C select(Predicate<TableColumnInfo> predicate) {
        return select(null, predicate);
    }

}
