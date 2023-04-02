package com.hcc.repository.extension.conditions.query;

import com.hcc.repository.extension.conditions.ChainCondition;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * ChainQuery
 *
 * @author hushengjun
 * @date 2023/4/2
 */
public interface ChainQuery<T, ID extends Serializable> extends ChainCondition<T, ID> {

    default List<T> list() {
        return getBaseMapper().selectList(getCondition());
    }

    default List<ID> listIds() {
        return getBaseMapper().selectIds(getCondition());
    }

    default <R> List<R> listObjects(Function<Object, R> mapper) {
        return getBaseMapper()
                .selectObjects(getCondition())
                .stream()
                .map(mapper)
                .collect(Collectors.toList());
    }

    default T one() {
        return getBaseMapper().selectOne(getCondition());
    }

    default Optional<T> oneOpt() {
        return Optional.ofNullable(this.one());
    }

    default Long count() {
        return getBaseMapper().selectCount(getCondition());
    }

}
