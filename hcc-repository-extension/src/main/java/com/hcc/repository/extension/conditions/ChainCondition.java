package com.hcc.repository.extension.conditions;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.mapper.BaseMapper;

import java.io.Serializable;

/**
 * ChainCondition
 *
 * @author hushengjun
 * @date 2023/4/2
 */
public interface ChainCondition<T, ID extends Serializable> {

    BaseMapper<T, ID> getBaseMapper();

    ICondition<T> getCondition();

}
