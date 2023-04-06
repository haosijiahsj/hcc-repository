package com.hcc.repository.extension.conditions.update;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
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
        ICondition<T> condition = getCondition();
        condition.setExecuteSqlType(ExecuteSqlTypeEnum.DELETE);
        return getBaseMapper().delete(condition) >= 0;
    }

}
