package com.hcc.repository.core.conditions.interfaces;

import java.util.function.Consumer;

/**
 * 嵌套的sql
 *
 * @author hushengjun
 * @date 2023/3/20
 */
public interface NestedClause<P, C> {

    C and(boolean condition, Consumer<P> consumer);

    default C and(Consumer<P> consumer) {
        return and(true, consumer);
    }

    C or(boolean condition, Consumer<P> consumer);

    default C or(Consumer<P> consumer) {
        return or(true, consumer);
    }

    C nested(boolean condition, Consumer<P> consumer);

    default C nested(Consumer<P> consumer) {
        return nested(true, consumer);
    }

}
