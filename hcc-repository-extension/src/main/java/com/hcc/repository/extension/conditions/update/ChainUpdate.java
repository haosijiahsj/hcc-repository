package com.hcc.repository.extension.conditions.update;

import com.hcc.repository.extension.conditions.ChainCondition;

import java.io.Serializable;

/**
 * ChainUpdate
 *
 * @author hushengjun
 * @date 2023/4/2
 */
public interface ChainUpdate<T, ID extends Serializable> extends ChainCondition<T, ID> {

    default boolean update() {
        return getBaseMapper().update(getCondition()) >= 0;
    }

    default boolean remove() {
        return getBaseMapper().delete(getCondition()) >= 0;
    }

}
